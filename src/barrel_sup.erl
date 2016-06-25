%%%-------------------------------------------------------------------
%% @doc barrel_core top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('barrel_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([status/0]).
-export([boot_status/0]).

%% Supervisor callbacks
-export([init/1]).

-include("log.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
    {ok, Pid} ->
      boot_status(),
      write_uri_file(),
      {ok, Pid};
    Else ->
      Else
  end.

status() ->
  #{ api => barrel_api_http:web_uris(),
     console => case barrel_server:get_env(start_console) of
                  false -> not_started;
                  true -> barrel_http_console:admin_uri()
                end
  }.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  _ = init_tabs(),

  barrel_config:init_config(),
  barrel_server:process_config(barrel_server:env()),



  Event = {barrel_event,
    {barrel_event, start_link, []},
    permanent,brutal_kill,	worker,[barrel_event]},

  UUIDs = {barrel_uuids,
           {barrel_uuids, start_link, []},
           permanent, brutal_kill, worker, [barrel_uuids]},

  TaskStatus = {barrel_task_status,
    {barrel_task_status, start_link, []},
    permanent,brutal_kill,	worker,[barrel_task_status]},

  ExtSup = {barrel_ext_sup,
            {barrel_ext_sup, start_link, []},
            permanent, infinity, supervisor, [barrel_ext_sup]},

  SafeSup =
    {barrel_safe_sup,
      {supervisor, start_link, [{local, baffel_safe_sup}, ?MODULE, safe]},
      permanent, infinity, supervisor, [?MODULE]},

  Replicator = {couch_replicator_sup,
    {couch_replicator_sup, start_link, []},
    permanent, infinity, supervisor, [couch_replicator_sup]},


  Server = {barrel_server,
            {barrel_server, start_link, []},
            permanent,brutal_kill,	worker,[barrel_server]},

  Metrics = {barrel_metrics_sup,
    {barrel_metrics_sup, start_link, []},
    permanent, infinity, supervisor, [barrel_metrics_sup]},

  UI = {barrel_ui_sup,
    {barrel_ui_sup, start_link, []},
    permanent, infinity, supervisor, [barrel_ui_sup]},

  QS =
    {couch_query_servers,
      {couch_query_servers, start_link, []},
      permanent, brutal_kill, worker, [couch_query_servers]},


  {ok, { {one_for_all, 0, 10}, [Event, UUIDs, TaskStatus, Server, QS, Replicator, Metrics, UI,
    SafeSup, ExtSup] } };

init(safe) ->
  SupFlags = {one_for_one, 4, 3600},


  Index = {couch_index_sup,
    {couch_index_sup, start_link, []},
    permanent, infinity, supervisor, [couch_index_sup]},

  ReplicationDaemon =
    {barrel_compaction_daemon,
      {barrel_compaction_daemon, start_link, []},
      permanent, brutal_kill, worker, [barrel_compaction_daemon]},


  {ok, {SupFlags, [Index, ReplicationDaemon]}}.

%%====================================================================
%% Internal functions
%%====================================================================


init_tabs() ->
  _ = ets:new(barrel_gvar, [set, named_table, public, {read_concurrency, true}]),
  ok.

boot_status() ->
  io:format("~n~n==> Barrel node started~n", []),
  io:format("version: ~s~n", [barrel_server:version()]),
  io:format("node id: ~s~n", [barrel_server:node_id()]),
  Status = status(),
  display_uris(maps:get(api, Status)),
  io:format("CONSOLE: ~s~n", [maps:get(console, Status)]).

display_uris(URIs) ->
  [io:format("HTPP API: ~s~n", [URI]) || URI <- URIs].

write_uri_file() ->
  case barrel_server:get_env(uri_file) of
    undefined -> ok;
    Filepath ->
      Lines = [io_lib:format("~s~n", [URI]) || URI <- barrel_api_http:web_uris()],
      case file:write_file(Filepath, Lines) of
        ok -> ok;
        {error, eacces} ->
          ?log(info, "Permission error when writing to URI file ~s", [Filepath]),
          throw({file_permission_error, Filepath});
        Error ->
          ?log(info, "Failed to write to URI file ~s: ~p~n", [Filepath, Error]),
          throw(Error)
      end
  end.
