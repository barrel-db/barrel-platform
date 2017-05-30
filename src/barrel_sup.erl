%% Copyright 2016-2017, Benoit Chesneau
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

-module(barrel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("barrel.hrl").

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
  %% init metrics
  %% TODO: make it optionnal
  barrel_prometheus:init(),

  StoreSup =
    #{id => barrel_store_sup,
      start => {barrel_store_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [barrel_store_sup]},

  Event = #{id => barrel_event,
            start => {barrel_event, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [barrel_event]},

  ReplicateSup =
    #{id => barrel_replicate_sup,
      start => {barrel_replicate_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [barrel_replicate_sup]},

  HttpSup =
    #{id => barrel_http_sup,
      start => {barrel_http_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [barrel_http_sup]},

  Specs = [
    StoreSup,
    Event,
    ReplicateSup,
    HttpSup
  ],
  
  {ok, { {one_for_one, 4, 3600}, Specs} }.
