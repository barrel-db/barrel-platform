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
      boot_status(),
      {ok, Pid};
    Else ->
      Else
  end.



%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  _ = init_tabs(),

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

  %% safe supervisor, like kernel_safe_sup but for barrel, allows to register
  %% external applications to it like stores if needed.
  ExtSup = {barrel_ext_sup,
            {barrel_ext_sup, start_link, []},
            permanent, infinity, supervisor, [barrel_ext_sup]},

  Server = {barrel_server,
            {barrel_server, start_link, []},
            permanent,brutal_kill,	worker,[barrel_server]},

  Daemons = {barrel_daemons_sup,
             {barrel_daemons_sup, start_link, []},
             permanent, infinity, supervisor, [barrel_daemons_sup]},

  Index = {couch_index_sup,
           {couch_index_sup, start_link, []},
           permanent, infinity, supervisor, [couch_index_sup]},

  Replicator = {couch_replicator_sup,
                {couch_replicator_sup, start_link, []},
                permanent, infinity, supervisor, [couch_replicator_sup]},

  Metrics = {barrel_metrics_sup,
             {barrel_metrics_sup, start_link, []},
             permanent, infinity, supervisor, [barrel_metrics_sup]},

  UI = {barrel_ui_sup,
         {barrel_ui_sup, start_link, []},
         permanent, infinity, supervisor, [barrel_ui_sup]},


  {ok, { {one_for_all, 0, 10}, [Event, UUIDs, TaskStatus, ExtSup, Metrics, Server, Daemons,
                                UI, Index, Replicator] } }.

%%====================================================================
%% Internal functions
%%====================================================================


init_tabs() ->
  _ = ets:new(barrel_gvar, [set, named_table, public, {read_concurrency, true}]),
  ok.

boot_status() ->
  URIs = barrel_api_http:web_uris(),
  io:format("~n~n==> Barrel node started~n", []),
  io:format("version: ~s~n", [barrel_server:version()]),
  io:format("node id: ~s~n", [barrel_server:node_id()]),
  display_uris(URIs),
  case barrel_http_console:is_enabled() of
    true -> io:format("ADMIN: ~s~n", [barrel_http_console:admin_uri()]);
    false -> ok
  end,
  write_uri_file(URIs).

display_uris(URIs) ->
  [io:format("HTPP API: ~s~n", [URI]) || URI <- URIs].

write_uri_file(URIs) ->
  case barrel_server:get_env(uri_file) of
    undefined -> ok;
    Filepath ->
      Lines = [io_lib:format("~s~n", [URI]) || URI <- URIs],
      case file:write_file(Filepath, Lines) of
        ok -> ok;
        {error, eacces} ->
          lager:info("Permission error when writing to URI file ~s", [Filepath]),
          throw({file_permission_error, Filepath});
        Error ->
          lager:info("Failed to write to URI file ~s: ~p~n", [Filepath, Error]),
          throw(Error)
      end
  end.
