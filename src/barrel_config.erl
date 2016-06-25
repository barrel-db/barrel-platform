%% Copyright (c) 2016. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Created by benoitc on 22/06/16.

-module(barrel_config).
-author("Benoit Chesneau").

%% API
-export([config_file/0]).
-export([init_config/0]).
-export([read_file/1]).

-include("barrel.hrl").

-define(DEFAULT_CONFIG, "barrel.yml").

config_file() ->
  case init:get_argument(config_file) of
    {ok, [[P]]} -> P;
    _ ->
      case application:get_env(barrel, config_file) of
        undefined -> filename:absname(?DEFAULT_CONFIG);
        {ok, P} -> P
      end
  end.

init_config() ->
  case read_file(config_file()) of
    {ok, Config} ->
      process_config(Config),
      maybe_init_logs();
    {error, Error} ->
      error_logger:error_msg(Error, []),
      erlang:error(Error)
  end.

read_file(Name) ->
  case fast_yaml:decode_from_file(Name, [plain_as_atom]) of
    {ok, []} -> {ok, []};
    {ok, [Document|_]} ->{ok, parserl(Document)};
    {error, enoent} -> {ok, []};
    Error -> Error
  end.

%% start logs for our own releases
maybe_init_logs() ->
  case ?catch_val(lager) of
    {'EXIT', _} -> ok;
    LogConfig ->
      case application:get_application(lager) of
        undefined ->
          error_logger:info_msg("lager not started: ~n"),
          ok;
        _ ->
          init_lager_config(LogConfig)
      end
  end.

init_lager_config([{lager_console_backend, Level} | Rest]) ->
  lager:set_loglevel(lager_console_backend, Level),
  init_lager_config(Rest);
init_lager_config([{lager_file_backend, Opts} | Rest]) ->
  case proplists:get_value(file, Opts) of
    undefined -> ok;
    File ->
      Opts2 = lists:foldl(fun
                            ({Key, Val}, Acc) when is_binary(Val) ->
                              [{Key, binary_to_list(Val)} | Acc];
                            ({Key, Val}, Acc) ->
                              [{Key, Val} | Acc]
                          end, [], Opts),
      HandlerId = {lager_file_backend, File},
      {ok, _} = supervisor:start_child(lager_handler_watcher_sup, [lager_event, HandlerId, Opts2])
  end,
  init_lager_config(Rest);
init_lager_config([]) ->
  ok.



%% TODO: add validation
process_config([{dir, Val} | Rest]) ->
  barrel_lib:set(dir, barrel_lib:to_list(Val)),
  process_config(Rest);
process_config([{config_dir, Val} | Rest]) ->
  barrel_lib:set(config_dir, barrel_lib:to_list(Val)),
  process_config(Rest);
process_config([{include_dir, Val} | Rest]) ->
  IncludeDir = filename:absname(barrel_lib:to_list(Val)),
  barrel_lib:set(include_dir, IncludeDir),
  case filelib:is_dir(IncludeDir) of
    true ->
      ConfFiles = filelib:wildcard("*.yml", IncludeDir),
      process_files(ConfFiles, IncludeDir);
    false ->
      error_logger:info_msg("include dir ~s not found.~n", [IncludeDir]),
      ok
  end,
  process_config(Rest);
process_config([{Key, Val} | Rest]) ->
  barrel_lib:set(Key, Val),
  process_config(Rest);
process_config([]) ->
  ok.


process_files([File | Rest], IncludeDir) ->
  case read_file(filename:join(IncludeDir, File)) of
    {ok, Config} ->
      process_config(Config),
      process_files(Rest, IncludeDir);
    {error, _Error} ->
      error_logger:info_msg("Error loading the config from ~s.~n", [File]),
      process_files(Rest, IncludeDir)
  end;
process_files([], _IncludeDir) ->
  ok.


parserl(<<"> ", Term/binary>>) ->
  {ok, A2, _} = erl_scan:string(binary_to_list(Term)),
  {ok, A3} = erl_parse:parse_term(A2),
  A3;
parserl({A, B}) ->
  {parserl(A), parserl(B)};
parserl([El|Tail]) ->
  [parserl(El) | parserl(Tail)];
parserl(Other) ->
  Other.
