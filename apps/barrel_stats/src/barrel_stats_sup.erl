%%%-------------------------------------------------------------------
%% @doc barrel_stats top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(barrel_stats_sup).

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
  Counters = child_spec(worker, counters, barrel_stats_counter, permanent, []),
  Histograms = child_spec(worker, histograms, barrel_stats_histogram, permanent, []),
  {ok, { {one_for_all, 0, 1}, [Counters, Histograms]} }.

%%====================================================================
%% Internal functions
%%====================================================================

child_spec(WorkerOrSupervisor, N, I, Restart, Args) ->
    #{
        id => N,
        start => {I, start_link, Args},
        restart => Restart,
        shutdown => 5000,
        type => WorkerOrSupervisor,
        modules => [I]
    }.
