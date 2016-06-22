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
      process_config(Config);
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

%% TODO: add validation
process_config([{dir, Val} | Rest]) ->
  barrel_lib:set(dir, barrel_lib:to_list(Val)),
  process_config(Rest);
process_config([{config_dir, Val} | Rest]) ->
  barrel_lib:set(config_dir, barrel_lib:to_list(Val)),
  process_config(Rest);
process_config([{Key, Val} | Rest]) ->
  barrel_lib:set(Key, Val),
  process_config(Rest);
process_config([]) ->
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
