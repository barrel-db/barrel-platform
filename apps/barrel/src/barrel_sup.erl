%%%-------------------------------------------------------------------
%% @doc barrel_core top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('barrel_sup').

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

    UUIDs = {barrel_uuids,
             {barrel_uuids, start_link, []},
             permanent, brutal_kill, worker, [barrel_uuids]},

    Log = {barrel_log,
           {barrel_log, start_link, []},
           permanent, brutal_kill, worker, [barrel_log]},

    {ok, { {one_for_all, 0, 1}, [UUIDs, Log]} }.

%%====================================================================
%% Internal functions
%%====================================================================
