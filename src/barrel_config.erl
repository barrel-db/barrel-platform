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
-export([start/0]).

-export([env/0, get_env/1, set_env/2, process_env/1]).

-export([config_file/0]).
-export([init_config/0]).
-export([read_file/1]).

-include("barrel.hrl").

-define(DEFAULT_CONFIG, "barrel.yml").

start() ->
  _ = ets:new(barrel_gvar, [set, named_table, public, {read_concurrency, true}]),
  init_config(),
  process_env(env()),
  ok.

process_env([]) -> ok;
process_env([E | Rest]) ->
  V = get_env(E),
  barrel_lib:set(E, V),
  process_env(Rest).

env() ->
  [
    dir,
    config_dir,
    extensions,
    uri_file,
    delayed_commits,
    fsync_options,
    file_compression,
    max_document_size,
    attachment_stream_buffer_size,
    attachment_compressible_types,
    attachment_compression_level,
    doc_buffer_size,
    checkpoint_after,
    stem_interactive_updates,
    listen,
    start_console,
    x_forwarded_host,
    x_forwarded_ssl,
    x_forwarded_proto,
    allow_jsonp,
    'WWW-Authenticate',
    changes_timeout,
    authentication_redirect,
    enable_cors,
    cors,
    require_valid_user,
    auth_handlers,
    auth_timeout,
    allows_persistent_cookie,
    auth_pbkdf2_iterations,
    auth_cache_size,
    auth_public_fields,
    users_db_public,
    os_process_timeout,
    reduce_limit,
    os_process_limit,
    query_servers,
    uuid_algorithm,
    utc_id_suffix,
    index_commit_freq,
    index_threshold,
    index_refresh_interval,
    keyvalue_buffer_size,
    min_writer_items,
    min_writer_size,
    request_timeout,
    max_replication_retry_count,
    worker_processes,
    worker_batch_size,
    http_connections,
    connection_timeout,
    retries_per_request,
    use_checkpoints,
    checkpoint_interval,
    socket_options,
    replicator_sslopts,
    replicator_cafile,
    compactions,
    compaction_check_interval,
    compaction_min_file_size
  ].


default_env(dir) ->
  case init:get_argument(barrel_dir) of
    {ok, [[D]]} -> D;
    _ ->
      Name = lists:concat(["data.", node()]),
      filename:absname(Name)
  end;
default_env(config_dir) ->
  case init:get_argument(config_dir) of
    {ok, [[D]]} -> D;
    _ -> undefined
  end;
default_env(extensions) ->
  [];
default_env(uri_file) ->
  undefined;
default_env(delayed_commits) ->
  false;
default_env(fsync_options) ->
  [before_header, after_header, on_file_open];
default_env(file_compression) ->
  snappy;
default_env(max_document_size) ->
  4294967296;
default_env(attachment_stream_buffer_size) ->
  4096;
default_env(attachment_compressible_types) ->
  [];
default_env(attachment_compression_level) ->
  0;
default_env(doc_buffer_size) ->
  524288;
default_env(checkpoint_after) ->
  524288 * 10;
default_env(stem_interactive_updates) ->
  true;
default_env(listen) ->
  [];
default_env(start_console) ->
  false;
default_env(x_forwarded_host) ->
  <<"x-forwarded-host">>;
default_env(x_forwarded_ssl) ->
  "X-Forwarded-Ssl";
default_env(x_forwarded_proto) ->
  "X-Forwarded-Proto";
default_env(allow_jsonp) ->
  false;
%%TODO: deprecate this option
default_env('WWW-Authenticate') ->
  nil;
%%TODO: deprecate this option
default_env(authentication_redirect) ->
  nil;
%%TODO: deprecate this option
default_env(changes_timeout) ->
  60000;
default_env(cors) ->
  [];
default_env(enable_cors) ->
  false;
default_env(require_valid_user) ->
  false;
default_env(auth_handlers) ->
  [barrel_basic_auth, barrel_cookie_auth];
default_env(allows_persistent_cookie) ->
  false;
default_env(auth_pbkdf2_iterations) ->
  10000;
default_env(auth_timeout) ->
  600;
default_env(auth_cache_size) ->
  50;
default_env(auth_public_fields) ->
  [];
default_env(users_db_public) ->
  false;
default_env(os_process_timeout) ->
  5000;
default_env(reduce_limit) ->
  true;
default_env(os_process_limit) ->
  25;
default_env(query_servers) ->
  [
    {<<"javascript">>, {couch_couchjs, start_link, [javascript]}},
    {<<"erlang">>, {couch_native_process, start_link, []}}
  ];
default_env(uuid_algorithm) ->
  sequential;
default_env(utc_id_suffix) ->
  "";
default_env(index_commit_freq) ->
  5;
default_env(index_threshold) ->
  200;
default_env(index_refresh_interval) ->
  1000;
default_env(keyvalue_buffer_size) ->
  2097152;
default_env(min_writer_items) ->
  100;
default_env(min_writer_size) ->
  16777216;
default_env(request_timeout) ->
  infinity;
default_env(max_replication_retry_count) ->
  10;
default_env(worker_processes) ->
  4;
default_env(worker_batch_size) ->
  500;
default_env(http_connections) ->
  20;
default_env(connection_timeout) ->
  30000;
default_env(retries_per_request) ->
  10;
default_env(use_checkpoints) ->
  true;
default_env(checkpoint_interval) ->
  5000;
default_env(socket_options) ->
  [{keepalive, true}, {nodelay, false}];
default_env(replicator_sslopts) ->
  [];
default_env(replicator_cafile) ->
  "";
default_env(compactions) ->
  [];
default_env(compaction_check_interval) ->
  300;
default_env(compaction_min_file_size) ->
  131072.

set_env(E, Val) -> barrel_lib:set(E, Val).

get_env(auth_secret) -> barrel_auth:secret();
get_env(config_dir) ->
  case ?catch_val(config_dir) of
    {'EXIT', _} ->
      case application:get_env(barrel, config_dir, default_env(config_dir)) of
        undefined ->
          Dir = filename:join(get_env(dir), ".barrel"),
          filelib:ensure_dir(filename:join(Dir, "dummy")),
          set_env(config_dir, Dir),
          Dir;
        Val ->
          Val
      end;
    Val -> Val
  end;
get_env(E) ->
  case ?catch_val(E) of
    {'EXIT', _} -> application:get_env(barrel, E, default_env(E));
    Val -> Val
  end.


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
