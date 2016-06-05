%%%-------------------------------------------------------------------
%% @doc barrel_core public API
%% @end
%%%-------------------------------------------------------------------

-module('barrel_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

-define(CONF_FILES, ["barrel.ini", "local.ini"]).

%%====================================================================
%% API
%%====================================================================

-spec start(application:start_type(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  Args = init:get_arguments(),
  ok = init_config(Args),
  ok = maybe_set_pidfile(Args),
  'barrel_sup':start_link().


-spec stop(atom()) -> ok.
stop(_State) ->
    ok.



%%====================================================================
%% Internal functions
%%====================================================================



init_config(Args) ->
    IniFiles = case proplists:get_value(inifiles, Args) of
                   undefined ->
                       ConfDir =  filename:join([code:root_dir(), "etc"]),
                       ConfigFiles = application:get_env(barrel, config_files, ?CONF_FILES),
                       lists:map(fun(FName) ->
                                         filename:join(ConfDir, FName)
                                 end, ConfigFiles);
                   [IniFilesStr] ->
                       re:split(IniFilesStr, "\\s*,||s*", [{return, list}])
               end,
    barrel_config:init(IniFiles).


maybe_set_pidfile(Args) ->
    case proplists:get_value(pidfile, Args) of
        undefined -> ok;
        [PidFile] ->
            case file:write_file(PidFile, os:getpid()) of
                ok ->
                    ok;
                {error, Reason} ->
                    error_logger:error_msg("Failed to write PID file ~s: ~s", [PidFile, Reason]),
                    ok
            end
    end.
