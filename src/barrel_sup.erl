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
  case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
    {ok, Pid} ->
      io:format("version: ~s.", [get_version()]),
      couch_httpd_util:display_uris(), %% display uris
      {ok, Pid};
    Else ->
      Else
  end.



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

  Server = {couch_server,
    {couch_server, sup_start_link, []},
    permanent,
    brutal_kill,
    worker,
    [couch_server]},

  Daemons = {barrel_daemons_sup,
    {barrel_daemons_sup, start_link, []},
    permanent, infinity, supervisor, [barrel_daemons_sup]},

    Http = {couch_httpd_sup,
      {couch_httpd_sup, start_link, []},
      permanent, infinity, supervisor, [couch_httpd_sup]},

  Index = {couch_index_sup,
    {couch_index_sup, start_link, []},
    permanent, infinity, supervisor, [couch_index_sup]},

  Replicator = {couch_replicator_sup,
    {couch_replicator_sup, start_link, []},
    permanent, infinity, supervisor, [couch_replicator_sup]},

  Metrics = {barrel_metrics_sup,
    {barrel_metrics_sup, start_link, []},
    permanent, infinity, supervisor, [barrel_metrics_sup]},

    {ok, { {one_for_all, 0, 10}, [UUIDs, Log, Metrics, Server, Daemons, Http, Index, Replicator]} }.

%%====================================================================
%% Internal functions
%%====================================================================

get_version() ->
  case application:get_key(barrel, vsn) of
    {ok, FullVersion} ->
      hd(string:tokens(FullVersion, "-"));
    _ ->
      "0.0.0"
  end.
