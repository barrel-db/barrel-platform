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

-module(barrel_store_sup).
-author("Benoit Chesneau").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-export([start_store/2, stop_store/1]).

-define(SHUTDOWN, 120000).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  Stores = application:get_env(barrel, stores, []),
  Specs = lists:map(
    fun({Name, Options}) ->
      store_spec(Name,Options)
    end,
    Stores
  ),
  {ok, {{one_for_one, 10, 60}, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private

-spec start_store(atom(), map()) -> supervisor:startchild_ret().
start_store(Name, Options) ->
  case supervisor:start_child(?MODULE, store_spec(Name, Options)) of
    {ok, _Pid} -> ok;
    {error, {already_started, _Pid}} -> ok;
    Error -> Error
  end.

-spec stop_store(term()) -> ok |{error, term()}.
stop_store(Name) ->
  case supervisor:terminate_child(?MODULE, Name) of
    ok ->
      _ = supervisor:delete_child(?MODULE, Name),
      %% unregister the store
      ets:delete(barrel_stores, Name),
      ok;
    {error, not_found} -> ok;
    Error ->
      Error
  end.

store_spec(Name, StoreOpts) ->
  Mod = maps:get(adapter, StoreOpts, barrel_rocksdb),
  %% register the store
  ets:insert(barrel_stores, {Name, Mod}),
  Opts = maps:get(adapter_options, StoreOpts, #{}),
  [Restart, Shutdown, Type, Modules] =
    [ maps:get(K, Opts, Default)
      || {K, Default} <- [{restart, transient},
                          {shutdown, ?SHUTDOWN},
                          {type, worker},
                          {modules, [Mod]}]
    ],
  #{
    id => Name,
    start => {Mod, start_link, [Name, StoreOpts]},
    restart => Restart,
    shutdown => Shutdown,
    type => Type,
    modules => Modules
  }.
