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

-module(couch_log).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

% public API
-export([start_link/0, stop/0]).
-export([debug/2, info/2, warn/2, error/2]).
-export([debug_on/0, info_on/0, warn_on/0, get_level/0, get_level_integer/0, set_level/1]).
-export([debug_on/1, info_on/1, warn_on/1, get_level/1, get_level_integer/1, set_level/2]).
-export([read/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(LEVEL_ERROR, 4).
-define(LEVEL_WARN, 3).
-define(LEVEL_INFO, 2).
-define(LEVEL_DEBUG, 1).

-record(state, {
    log_file,
    level,
    sasl
}).

debug(Format, Args) ->
    lager:debug(Format, Args).

info(Format, Args) ->
    lager:info(Format, Args).

warn(Format, Args) ->
    lager:warning(Format, Args).

error(Format, Args) ->
    lager:error(Format, Args).


level_integer(error)    -> ?LEVEL_ERROR;
level_integer(warn)     -> ?LEVEL_WARN;
level_integer(info)     -> ?LEVEL_INFO;
level_integer(debug)    -> ?LEVEL_DEBUG;
level_integer(_Else)    -> ?LEVEL_ERROR. % anything else default to ERROR level

level_atom(?LEVEL_ERROR) -> error;
level_atom(?LEVEL_WARN) -> warn;
level_atom(?LEVEL_INFO) -> info;
level_atom(?LEVEL_DEBUG) -> debug.

debug_on() ->
    get_level_integer() =< ?LEVEL_DEBUG.

info_on() ->
    get_level_integer() =< ?LEVEL_INFO.

warn_on() ->
    get_level_integer() =< ?LEVEL_WARN.

debug_on(Module) ->
    get_level_integer(Module) =< ?LEVEL_DEBUG.

info_on(Module) ->
    get_level_integer(Module) =< ?LEVEL_INFO.

warn_on(Module) ->
    get_level_integer(Module) =< ?LEVEL_WARN.

set_level(LevelAtom) ->
    set_level_integer(level_integer(LevelAtom)).

set_level(Module, LevelAtom) ->
    set_level_integer(Module, level_integer(LevelAtom)).

get_level() ->
    level_atom(get_level_integer()).

get_level(Module) ->
    level_atom(get_level_integer(Module)).

get_level_integer() ->
    try
        ets:lookup_element(?MODULE, level, 2)
    catch error:badarg ->
        ?LEVEL_ERROR
    end.

get_level_integer(Module0) ->
    Module = atom_to_list(Module0),
    try
        [{_Module, Level}] = ets:lookup(?MODULE, Module),
        Level
    catch error:_ ->
        get_level_integer()
    end.

set_level_integer(Int) ->
    gen_event:call(error_logger, couch_log, {set_level_integer, Int}).

set_level_integer(Module, Int) ->
    gen_event:call(error_logger, couch_log, {set_level_integer, Module, Int}).



start_link() ->
    gen_server:start_link({local, couch_log}, couch_log, [], []).

stop() ->
    couch_event_sup:stop(couch_log).

init([]) ->
    % read config and register for configuration changes
    ok = couch_config:register(fun
                ("log", "file") ->
                    gen_server:cast(?MODULE, config_update);
                ("log", "level") ->
                    gen_server:cast(?MODULE, config_update);
                ("log", "include_sasl") ->
                    gen_server:cast(?MODULE, {config_update, include_sasl});
                ("log_level_by_module", _) ->
                    gen_server:cast(?MODULE,
                                    {config_update, log_level_by_module})
            end),


    Filename = log_file(),
    ALevel = list_to_atom(couch_config:get("log", "level", "info")),
    Level = level_integer(ALevel),
    Sasl = couch_config:get("log", "include_sasl", "true") =:= "true",
    LevelByModule = couch_config:get("log_level_by_module"),

    %% maybe start the log file backend
    maybe_start_logfile_backend(Filename, ALevel),

    %% initialise the ets table if needed
    case ets:info(?MODULE) of
        undefined -> ets:new(?MODULE, [named_table]);
        _ -> ok
    end,

    %% set the default level
    ets:insert(?MODULE, {level, Level}),

    %% initialise the log level by modules
    lists:foreach(fun({Module, ModuleLevel}) ->
        ModuleLevelInteger = level_integer(list_to_atom(ModuleLevel)),
        ets:insert(?MODULE, {Module, ModuleLevelInteger})
    end, LevelByModule),

    %% set default log level
    set_loglevel(Filename, ALevel),

    {ok, #state{log_file=Filename, level = Level, sasl = Sasl}}.

handle_call({set_level_integer, NewLevel}, _From, State) ->
    ets:insert(?MODULE, {level, NewLevel}),
    {ok, ok, State#state{level = NewLevel}};

handle_call({set_level_integer, Module, NewLevel}, _From, State) ->
    ets:insert(?MODULE, {Module, NewLevel}),
    {ok, ok, State#state{level = NewLevel}}.

handle_cast(config_update, #state{log_file=OldFilename}=State) ->
    Filename = log_file(),
    ALevel = list_to_atom(couch_config:get("log", "level", "info")),
    Level = level_integer(ALevel),

    %% set default module
    ets:insert(?MODULE, {level, Level}),

    %% should we restart the file backend with a new config?
    if OldFilename =/= Filename ->
            restart_logfile_backend(OldFilename, Filename, ALevel);
        true -> ok
    end,

    %% set log level
    set_loglevel(Filename, ALevel),

    {noreply, State#state{log_file=Filename, level = Level}};

handle_cast({config_update, include_sasl}, State) ->
    Sasl = couch_config:get("log", "include_sasl", "true") =:= "true",
    {noreply, State#state{sasl=Sasl}};
handle_cast({config_update, log_level_by_module}, State) ->
    LevelByModule = couch_config:get("log_level_by_module"),
    lists:foreach(fun({Module, ModuleLevel}) ->
        ModuleLevelInteger = level_integer(list_to_atom(ModuleLevel)),
        ets:insert(?MODULE, {Module, ModuleLevelInteger})
    end, LevelByModule),

    {noreply, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.


% Read Bytes bytes from the end of log file, jumping Offset bytes towards
% the beginning of the file first.
%
%  Log File    FilePos
%  ----------
% |          |  10
% |          |  20
% |          |  30
% |          |  40
% |          |  50
% |          |  60
% |          |  70 -- Bytes = 20  --
% |          |  80                 | Chunk
% |          |  90 -- Offset = 10 --
% |__________| 100

read(Bytes, Offset) ->
    LogFileName = log_file(),
    LogFileSize = filelib:file_size(LogFileName),
    MaxChunkSize = list_to_integer(
        couch_config:get("httpd", "log_max_chunk_size", "1000000")),
    case Bytes > MaxChunkSize of
    true ->
        throw({bad_request, "'bytes' cannot exceed " ++
            integer_to_list(MaxChunkSize)});
    false ->
        ok
    end,

    {ok, Fd} = file:open(LogFileName, [read]),
    Start = lists:max([LogFileSize - Bytes - Offset, 0]),

    % TODO: truncate chopped first line
    % TODO: make streaming

    {ok, Chunk} = file:pread(Fd, Start, Bytes),
    ok = file:close(Fd),
    Chunk.


maybe_start_logfile_backend(Filename, Level) ->
    Started = case application:get_env(lager, handlers) of
        undefined -> false;
        {ok, Handlers} ->
            LogFiles = lists:foldl(fun
                    ({lager_file_backend, Config}, Acc) ->
                        [hfile(Config) | Acc];
                    (_, Acc) ->
                        Acc
                end, [], Handlers),
            lists:member(Filename, LogFiles)
    end,

    case Started of
        true -> ok;
        false ->
            Config = [{file, Filename},
                      {level, Level},
                      {formatter, lager_default_formatter},
                      {formatter_config,
                       ["[", time, "] [", pid, "] [", severity, "] ",
                        message, "\n"]}],
            HandlerId = {lager_file_backend, Filename},
            {ok, _} = supervisor:start_child(lager_handler_watcher_sup,
                                             [lager_event, HandlerId, Config])
    end.


restart_logfile_backend(OldFilename, Filename, Level) ->
    ok = gen_event:delete_handler(lager_event, {lager_file_backend,
                                                OldFilename}, []),

    %% restart a new handler
    Config = [{file, Filename},
              {level, Level},
              {formatter, lager_default_formatter},
              {formatter_config,
               ["[", time, "] [", pid, "] [", severity, "] ", message, "\n"]}],
    HandlerId = {lager_file_backend, Filename},
    {ok, _} = supervisor:start_child(lager_handler_watcher_sup,
                                     [lager_event, HandlerId, Config]),
    ok.

set_loglevel(Filename, ALevel) ->
    %% set default log level
    lager:set_loglevel(lager_console_backend, ALevel),
    lager:set_loglevel(lager_file_backend, Filename, ALevel),

    %% set the log level for other handlers
    case application:get_env(lager, handlers) of
        undefined -> ok;
        {ok, Handlers} ->
            lists:foreach(fun(Handler) ->
                        lager:set_loglevel(Handler, ALevel)
                end, Handlers)
    end.

log_file() ->
    DefaultLogFile = case application:get_env(couch, log_file) of
        undefined -> "couchdb.log";
        FName -> FName
    end,
    couch_config:get("log", "file", DefaultLogFile).

hfile({FileName, LogLevel}) when is_list(FileName), is_atom(LogLevel) ->
    %% backwards compatability hack
    FileName;
hfile({FileName, LogLevel, _Size, _Date, _Count})
        when is_list(FileName), is_atom(LogLevel) ->
    %% backwards compatability hack
    FileName;
hfile([{FileName, LogLevel, _Size, _Date, _Count}, {Formatter, _FormatterConfig}])
    when is_list(FileName), is_atom(LogLevel), is_atom(Formatter) ->
    %% backwards compatability hack
    FileName;
hfile([LogFile,{Formatter}]) ->
    %% backwards compatability hack
    hfile([LogFile,{Formatter,[]}]);
hfile([{FileName, LogLevel}, {Formatter, _FormatterConfig}])
    when is_list(FileName), is_atom(LogLevel), is_atom(Formatter) ->
    %% backwards compatability hack
   FileName;
hfile(LogFileConfig) when is_list(LogFileConfig) ->
    proplists:get_value(file, LogFileConfig).
