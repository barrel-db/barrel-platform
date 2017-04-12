%%%-------------------------------------------------------------------
%% @doc barrel_statsd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(barrel_statsd_sup).

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

  Statsd =
    #{id => barrel_statsd,
      start => {barrel_statsd, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [barrel_statsd]},

  {ok, { {one_for_all, 10000, 1}, [Statsd]} }.

%%====================================================================
%% Internal functions
%%====================================================================
