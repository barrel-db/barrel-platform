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

-export([start_db/3]).

-include("barrel.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  %% initialize the databases from the conf. If missing, create it.
  Dbs = application:get_env(barrel, dbs, []),
  Specs = lists:map(
    fun({Name, Options0}) ->
      {Id, Options1} = case barrel_store:whereis_db(Name) of
        undefined ->
          {barrel_keys:db_id(Name), Options0#{ create_if_missing => true}};
        #db{id=DbId} ->
          {DbId, Options0}
      end,
      db_spec(Name, Id, Options1)
    end,
    Dbs
  ),
  {ok, {{one_for_one, 10, 60}, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private

-spec start_db(binary(), binary(), map()) -> supervisor:startchild_ret().
start_db(Name, Id, Options) ->
  supervisor:start_child(?MODULE, db_spec(Name, Id, Options)).

db_spec(Name, Id, Options) ->
  #{
    id => Id,
    start => {barrel_db, start_link, [Name, Id, Options]},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [barrel_db]
  }.
