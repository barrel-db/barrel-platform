%%%-------------------------------------------------------------------
%% @doc barrel_monitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(barrel_monitor_sup).

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
    ok = barrel_monitor_perf:init_context(),

    %% initialize vm collector
    ok = prometheus_registry:register_collector(prometheus_vm_memory_collector),
    ok = prometheus_registry:register_collector(prometheus_vm_statistics_collector),
    ok = prometheus_registry:register_collector(prometheus_vm_system_info_collector),

    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
