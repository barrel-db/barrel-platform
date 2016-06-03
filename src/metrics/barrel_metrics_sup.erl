%% Copyright 2016 Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://joinup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

%%%-------------------------------------------------------------------
%% @doc barrel_metrics top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(barrel_metrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
  Track = {barrel_metrics_process,
    {barrel_metrics_process, start_link, []},
    permanent, brutal_kill, worker, [barrel_metrics_process]},

  Conf = {barrel_metrics,
    {barrel_metrics, start_link, []},
    permanent, brutal_kill, worker, [barrel_metrics]},

  {ok, { {one_for_all, 10, 3600}, [Track, Conf]} }.

%%====================================================================
%% Internal functions
%%====================================================================