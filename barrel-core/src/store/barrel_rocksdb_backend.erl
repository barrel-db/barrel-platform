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
-export([start_link/3]).

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

-spec start_link(atom(), atom(), list()) -> {ok, pid()}.
start_link(Backend, Name, Config) ->
  gen_server:start_link({local, Backend}, ?MODULE, [Name, Config], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init([Name, Config]) ->
  DefaultDir = filename:join(barrel_lib:data_dir(), Name),
  DbDir = maps:get(dir, Config, DefaultDir),
  filelib:ensure_dir(filename:join(DbDir, "empty")),
  InMemory = maps:get(in_memory, Config, false),
  DbOpts = case InMemory of
             true -> [{create_if_missing, true}, {in_memory, true}];
             false -> [{create_if_missing, true}]
           end,
  {ok, Db} = erocksdb:open(DbDir, DbOpts, []),
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
