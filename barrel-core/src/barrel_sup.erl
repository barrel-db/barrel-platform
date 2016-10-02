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

-module(barrel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(sup(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).

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
  SafeSup =
    {barrel_safe_sup,
     {supervisor, start_link, [{local, barrel_safe_sup}, ?MODULE, safe]},
     permanent, infinity, supervisor, [?MODULE]},
  
  Specs = [
    ?sup(barrel_ext_sup),
    SafeSup

  ],
  {ok, { {one_for_all, 0, 10}, Specs} };

init(safe) ->
  Specs =[ ?sup(barrel_stores_sup)
         , ?sup(barrel_dbs_sup)
         , ?sup(barrel_event)
         , ?sup(barrel_task_status)
         ],
  
  {ok, { {one_for_one, 5, 10}, Specs} }.

  
