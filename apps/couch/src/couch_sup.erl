% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_sup).
-behaviour(supervisor).


-export([start_link/1,stop/0, couch_config_start_link_wrapper/2,
        restart_core_server/0, config_change/2]).

-include("couch_db.hrl").

%% supervisor callbacks
-export([init/1]).

start_link(IniFiles) ->
    case whereis(couch_sup) of
    undefined ->
        start_server(IniFiles);
    _Else ->
        {error, already_started}
    end.

restart_core_server() ->
    init:restart().

couch_config_start_link_wrapper(IniFiles, FirstConfigPid) ->
    case is_process_alive(FirstConfigPid) of
        true ->
            link(FirstConfigPid),
            {ok, FirstConfigPid};
        false -> couch_config:start_link(IniFiles)
    end.

start_server(IniFiles) ->
    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
        ok -> ok;
        {error, Reason} ->
            io:format("Failed to write PID file ~s: ~s",
                [PidFile, file:format_error(Reason)])
        end;
    _ -> ok
    end,

    {ok, ConfigPid} = couch_config:start_link(IniFiles),

    LogLevel = couch_config:get("log", "level", "info"),
    % announce startup
    io:format("couch ~s (LogLevel=~s) is starting.~n", [
        couch_server:get_version(),
        LogLevel
    ]),
    case LogLevel of
    "debug" ->
        io:format("Configuration Settings ~p:~n", [IniFiles]),
        [io:format("  [~s] ~s=~p~n", [Module, Variable, Value])
            || {{Module, Variable}, Value} <- couch_config:all()];
    _ -> ok
    end,

    BaseChildSpecs =
    {{one_for_all, 10, 3600},
        [{couch_config,
            {couch_sup, couch_config_start_link_wrapper, [IniFiles, ConfigPid]},
            permanent,
            brutal_kill,
            worker,
            [couch_config]},
        {couch_primary_services,
            {couch_primary_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_primary_sup]},
        {couch_secondary_services,
            {couch_secondary_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_secondary_sup]}
        ]},

    % ensure these applications are running
    application:start(ibrowse),
    application:start(crypto),

    {ok, Pid} = supervisor:start_link(
        {local, couch_sup}, couch_sup, BaseChildSpecs),

    % launch the icu bridge
    % just restart if one of the config settings change.
    couch_config:register(fun ?MODULE:config_change/2, Pid),

    unlink(ConfigPid),

    {ok, Pid}.

stop() ->
    catch exit(whereis(couch_sup), normal).

config_change("daemons", _) ->
    supervisor:terminate_child(couch_sup, couch_secondary_services),
    supervisor:restart_child(couch_sup, couch_secondary_services).

init(ChildSpecs) ->
    {ok, ChildSpecs}.
