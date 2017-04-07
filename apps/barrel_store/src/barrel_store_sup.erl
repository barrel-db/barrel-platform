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

%%%-------------------------------------------------------------------
%% @doc barrel top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(barrel_store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("barrel_store.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  _ = ets:new(barrel_dbs, [ordered_set, named_table, public, {keypos, #db.id}]),

  DbSup = #{id => barrel_db_sup,
            start => {barrel_db_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [barrel_db_sup]},

  Store =
    #{
      id => barrel_store,
      start => {barrel_store, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [barrel_store]
    },

  Event = #{id => barrel_event,
            start => {barrel_event, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [barrel_event]},

  Status =  #{id => barrel_task_status,
              start => {barrel_task_status, start_link, []},
              restart => permanent,
              shutdown => infinity,
              type => worker,
              modules => [barrel_task_status]},



  Specs = [
      DbSup
    , Store
    , Event
    , Status
  ],

  {ok, { {one_for_one, 4, 3600}, Specs} }.
