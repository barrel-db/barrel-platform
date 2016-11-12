%% Copyright 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_transactor).
-author("Benoit Chesneau").
-behaviour(gen_statem).

%% API
-export([
  start_link/4,
  update_doc/4,
  last_update_seq/1,
  find/1
]).


%% gen_statem callbacks
-export([
  terminate/3,
  code_change/4,
  init/1,
  callback_mode/0
]).

%% state callbacks
-export([
  wait_for_transaction/3,
  process_transaction/3
]).

%% internal
-export([
  handle_write_doc/5
]).

-define(default_timeout, 5000).


%%%===================================================================
%%% API
%%%===================================================================


update_doc(DbId, DocId, Fun, Options) ->
  Async = proplists:get_value(async, Options, false),
  Timeout = proplists:get_value(timeout, Options, ?default_timeout),
  
  case Async of
    true ->
      gen_statem:cast(find(DbId), {update_doc, DocId, Fun, Options});
    false ->
      gen_statem:call(find(DbId), {update_doc, DocId, Fun, Options}, Timeout)
  end.


last_update_seq(DbId) ->
  case ets:lookup(?MODULE, DbId) of
    [] -> undefined;
    [{DbId, _Transactor, Seq}] -> Seq
  end.


find(DbId) ->
  case ets:lookup(?MODULE, DbId) of
    [] -> undefined;
    [{DbId, Transactor, _Seq}] -> Transactor
  end.


start_link(Parent, DbId, Store, UpdateSeq) ->
  gen_statem:start_link(?MODULE, [Parent, DbId, Store, UpdateSeq], []).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================


init([Parent, DbId, Store, UpdateSeq]) ->
  process_flag(trap_exit, true),
  Data = #{
    parent => Parent,
    dbid => DbId,
    store => Store,
    update_seq => UpdateSeq
  },
  true = ets:insert_new(?MODULE, {DbId, self(), UpdateSeq}),
  {ok, wait_for_transaction, Data}.


terminate(_Reason, _State, #{ dbid := DbId}) ->
  ets:delete(?MODULE, DbId),
  ok.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.

callback_mode() -> state_functions.

%%%===================================================================
%%% state callbacks
%%%===================================================================

%% Note: we spawn the transaction to isolate it from the main process
%% TODO: maybe we should implement a worker pool?
wait_for_transaction({call, From}, {update_doc, DocId, Fun, Options}, Data) ->
  Pid = spawn_link(?MODULE, handle_write_doc, [self(), DocId, Fun, Options, Data]),
  {next_state, process_transaction, Data#{ t => {Pid, From}}};

wait_for_transaction(cast, {update_doc, DocId, Fun, Options}, Data) ->
  Pid = spawn_link(?MODULE, handle_write_doc, [self(), DocId, Fun, Options, Data]),
  {next_state, process_transaction, Data#{ tx => {Pid, nil} }};

wait_for_transaction(_EventType, _Event, Data) ->
  {keep_state, Data}.

%% TODO: track transaction id instead of simpy postponing events
process_transaction(_EventType, {update_doc, _DocId, _Fun, _Options}, Data) ->
  {keep_state, Data, [postpone]};
process_transaction(info, {updated, Pid, Reply, Seq}, Data = #{ parent := Parent, t := {Pid, From}}) ->
  Parent ! {updated, Seq},
  {next_state, wait_for_transaction, Data#{ update_seq => Seq}, [{reply, From, Reply}]};
process_transaction(_EventType, _Event, Data) ->
  {keep_state, Data}.

%%%===================================================================
%%% internal ee
%%%===================================================================

empty_doc_info() ->
  #{ current_rev => <<>>, revtree => #{}}.

handle_write_doc(Parent, DocId, Fun, _Options, #{ dbid := DbId, store := Store, update_seq := Seq }) ->
  DocInfo = case barrel_store:get_doc_info(Store, DbId, DocId) of
              {ok, DI} -> DI;
              {error, not_found} -> empty_doc_info();
              Error -> throw(Error)
            end,
  
  case Fun(DocInfo) of
    {ok, DocInfo2, Body, NewRev} ->
      NewSeq = Seq + 1,
      LastSeq = maps:get(update_seq, DocInfo2, undefined),
      case barrel_store:write_doc(Store, DbId, DocId, LastSeq, DocInfo2#{update_seq => NewSeq}, Body) of
        ok ->
          _ = ets:update_counter(?MODULE, DbId, {3, 1}),
          Parent ! {updated, self(), {ok, DocId, NewRev}, NewSeq};
        WriteError ->
          lager:error("db error: error writing ~p on ~p", [DocId, DbId]),
          %% NOTE: should we retry?
          Parent ! {updated, self(), WriteError, Seq}
      end;
    ok ->
      %% NOTE: should we return a cancel message instead?
      #{ current_rev := Rev } = DocInfo,
      Parent ! {updated, self(), {ok, DocId, Rev}, Seq};
    Conflict ->
      Parent ! {updated, self(), {error, Conflict}, Seq}
  end.
