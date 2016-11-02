%% Copyright 2016, Bernard Notarianni
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

-module(barrel_metrics_SUITE_).
-author("Bernard Notarianni").

%% API
-export(
   [
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
   ]).

-export(
   [ replicate_ok/1
   , replicate_read_fail/1
   , replicate_write_fail/1
   ]).


all() ->
  [ replicate_ok
  , replicate_read_fail
  , replicate_write_fail
  ].

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
  Config.

init_per_testcase(_, Config) ->
  ok = barrel_db:start(target(), barrel_test_rocksdb, [{create_if_missing, true}]),
  ok = barrel_db:start(source(), barrel_source_rocksdb, [{create_if_missing, true}]),
  Config.

end_per_testcase(_, _Config) ->
  ok = barrel_replicate:clean(source(), target()),
  ok = barrel_db:clean(target()),
  ok = barrel_db:clean(source()),
  ok.

end_per_suite(Config) ->
  Config.

source() ->
  <<"source">>.

target() ->
  <<"testdb">>.

replicate_ok(_Config) ->
  Options = [{metrics_freq, 100}],
  {ok, RepId} = barrel:start_replication(source(), target(), Options),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(source(), <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  timer:sleep(200),
  {ok, Doc2} = barrel_db:get(target(), <<"a">>, []),
  
  [Stats] = barrel_task_status:all(),
  io:format("what are my stats? ~p~n", [Stats]),
  ok = barrel:stop_replication(RepId),
  
  1 = proplists:get_value(docs_read, Stats),
  1 = proplists:get_value(docs_written, Stats),

  ok.

replicate_read_fail(_Config) ->
  meck:new(barrel_db, [passthrough]),
  MeckGet = fun(_Db, _DocId, _Options) ->
                {error, generated_by_mock_from, ?MODULE}
            end,
  meck:expect(barrel_db, get, MeckGet),

  Options = [{metrics_freq, 100}],
  {ok, RepId} = barrel:start_replication(source(), target(), Options),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(source(), <<"a">>, Doc, []),
  timer:sleep(200),

  [Stats] = barrel_task_status:all(),
  ok = barrel:stop_replication(RepId),
  
  0 = proplists:get_value(docs_read, Stats),
  0 = proplists:get_value(docs_written, Stats),
  1 = proplists:get_value(doc_read_failures, Stats),
  
  meck:unload(barrel_db),
  ok.

replicate_write_fail(_Config) ->
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(source(), <<"a">>, Doc, []),
  meck:new(barrel_db, [passthrough]),
  MeckPutRev = fun(_Db, _Id, _Doc, _History, _Options) ->
                {error, from_mock}
            end,
  meck:expect(barrel_db, put_rev, MeckPutRev),

  Options = [{metrics_freq, 100}],
  {ok, RepId} = barrel:start_replication(source(), target(), Options),
  timer:sleep(200),

  [Stats] = barrel_task_status:all(),
  ok = barrel:stop_replication(RepId),
  1 = proplists:get_value(docs_read, Stats),
  0 = proplists:get_value(docs_written, Stats),
  0 = proplists:get_value(doc_read_failures, Stats),
  1 = proplists:get_value(doc_write_failures, Stats),
  
  
  
  meck:unload(barrel_db),
  
  ok.

