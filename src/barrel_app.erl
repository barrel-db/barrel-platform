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
  ok = maybe_set_pidfile(Args),
  barrel_config:start(),
  barrel_ext:start(),
  barrel_metrics:start(),
  'barrel_sup':start_link().

-spec stop(atom()) -> ok.
stop(_State) ->
  barrel_ext:stop(),
  ok.


%%====================================================================
%% Internal functions
%%====================================================================


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
