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

-module(barrel_db_sup).
-author("Benoit Chesneau").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-export([start_db/3, stop_db/1, await_db/1]).

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
  {ok, {{one_for_one, 5, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private

-spec start_db(term(), atom(), barrel_db:db_options()) -> supervisor:startchild_ret().
start_db(Name, Store, Options) ->
  supervisor:start_child(?MODULE, db_spec(Name, Store, Options)).

-spec stop_db(term()) -> ok |{error, term()}.
stop_db(Name) ->
  case supervisor:terminate_child(?MODULE, Name) of
    ok ->
      _ = supervisor:delete_child(?MODULE, Name),
      ok;
    {error, not_found} -> ok;
    Error ->
      Error
  end.

await_db(Name) ->
  _ = gproc:await({n, l, {barrel_db, Name}}, 5000),
  ok.

db_spec(Name, Store, Options) ->
  #{
    id => Name,
    start => {barrel_db, start_link, [Name, Store, Options]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [barrel_db]
  }.

