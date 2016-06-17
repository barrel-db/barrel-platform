%% Copyright (c) 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.
%%

-module(barrel_log).
-author("Benoit Chesneau").

%% API
-export([init/0]).
-export([log_level/1]).
-export([read/2]).


%% TODO: add syslog config & custom backends
init() ->
  init_error_log(),
  init_console_log(),
  ok.


init_error_log() ->
  case barrel_config:get("log", "error_log", "log/error.log") of
    undefined -> [];
    Filepath ->
      ErrorLevel = log_level(barrel_config:get("log", "error_level", "error")),
      Config = [{file, Filepath},
        {level, ErrorLevel},
        {size, 10485760},
        {count, 5}],
      HandlerId = {lager_file_backend, Filepath},
      {ok, _} = supervisor:start_child(lager_handler_watcher_sup, [lager_event, HandlerId, Config]),
      ok
  end.

init_console_log() ->
  Console = log_level(barrel_config:get("log", "console")),
  lager:set_loglevel(lager_console_backend, Console).

log_level(undefined) -> none;
log_level("none") -> none;
log_level("off") -> none;
log_level("debug") -> debug;
log_level("error") -> error;
log_level("warning") -> warning;
log_level("info") -> info;
log_level(_) -> error(badarg).

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
  LogFileName = barrel_config:get("log", "error_log", "log/error.log"),
  LogFileSize = filelib:file_size(LogFileName),
  MaxChunkSize = barrel_config:get_integer("httpd", "log_max_chunk_size", 1000000),
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
