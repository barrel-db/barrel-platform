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

%% Created by benoitc on 03/09/16.

-module(barrel_rocksdb_backend).
-author("Benoit Chesneau").
-behaviour(gen_server).

%% API

-export([get_db/1]).
-export([start_link/2]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {db :: any()}).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

get_db(Name) -> gen_server:call(Name, get_db).

-spec start_link(atom(), list()) -> {ok, pid()}.
start_link(Name, Options) ->
  BackendName = {n, l, {rocksdb_backend, Name}},
  gen_server:start_link({via, gproc, BackendName}, ?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init(Options) ->
  DbDir = proplists:get_value(dir, Options),
  {ok, Db} = erocksdb:open(DbDir, [{create_if_missing, true}], []),
  {ok, #state{db=Db}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(get_db, _From, State = #state{db=Db}) ->
  {reply, Db, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
  _ = erocksdb:close(State#state.db),
  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
