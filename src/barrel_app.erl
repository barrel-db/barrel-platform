%%%-------------------------------------------------------------------
%% @doc barrel_core public API
%% @end
%%%-------------------------------------------------------------------

-module('barrel_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

-define(DEFAULT_INI, "barrel.ini").

%%====================================================================
%% API
%%====================================================================

-spec start(application:start_type(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  Args = init:get_arguments(),
  ok = init_config(Args),
  ok = maybe_set_pidfile(Args),
  _ = barrel_log:init(),
  'barrel_sup':start_link().

-spec stop(atom()) -> ok.
stop(_State) ->
    ok.



%%====================================================================
%% Internal functions
%%====================================================================


init_config(Args) ->
  IniFiles = case proplists:get_value(inifiles, Args) of
               undefined -> application:get_env(barrel, config_files, []);
               [IniFilesStr] -> re:split(IniFilesStr, "\\s*,||s*", [{return, list}])
             end,

  IniFiles1 = [filename:absname(Ini) || Ini <- maybe_create_default(IniFiles)],
  ok = check_inifiles(IniFiles1),
  barrel_config:init(IniFiles1).


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

maybe_create_default([]) ->
  Filename = filename:absname(?DEFAULT_INI),
  case filelib:is_file(Filename) of
    true -> [Filename];
    false ->
      file:copy(filename:join(barrel_lib:priv_dir(), "local.ini"), Filename),
      [Filename]
  end;
maybe_create_default(IniFiles) -> IniFiles.

check_inifiles([]) -> ok;
check_inifiles([File | Rest]) ->
  case filelib:is_file(File) of
    false -> throw({invalid_config, File});
    true -> check_inifiles(Rest)
  end.
