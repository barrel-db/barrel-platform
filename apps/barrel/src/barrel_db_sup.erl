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

-export([create_db/2]).

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
  %% initialize persistent databases.
  {Dbs, Specs0} = persisted_databases(),
  %% final spec
  Specs = lists:foldl(
    fun({Name, Conf0}, Acc) ->
      case lists:member(Name, Dbs) of
        true ->
          %% we ignore already created database
          Acc;
        false ->
          {DbId, Conf}= case maps:is_key(db_id, Conf0) of
                          true -> maps:take(db_id, Conf0);
                          false -> {barrel_keys:db_id(Name), Conf0}
                        end,
          [db_spec(DbId, {create, Name, DbId, Conf}) | Acc]
      end
    end,
    Specs0,
    application:get_env(barrel, dbs, [])
  ),
  
  {ok, {{one_for_one, 10, 60}, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private

-spec create_db(binary(), map()) -> supervisor:startchild_ret().
create_db(Name, Conf0) ->
  {DbId, Conf}= case maps:is_key(db_id, Conf0) of
                  true -> maps:take(db_id, Conf0);
                  false -> {barrel_keys:db_id(Name), Conf0}
                end,
  supervisor:start_child(?MODULE, db_spec(DbId, {create, Name, DbId, Conf})).


db_spec(Id, Args) ->
  #{
    id => Id,
    start => {barrel_db, start_link, [Args]},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [barrel_db]
  }.

persisted_databases() ->
  barrel_store:fold_databases(
    fun(DbName, Meta, {Dbs, Specs}) ->
      DbId = maps:get(id, Meta),
      {ok, {[DbName | Dbs], [db_spec(DbId, {open, DbName, Meta}) | Specs]}}
    end,
    {[], []}
  ).
