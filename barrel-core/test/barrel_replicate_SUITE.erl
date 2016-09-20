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

-module(barrel_replicate_SUITE).
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
   [
    one_doc/1,
    target_not_empty/1,
    deleted_doc/1,
    over_http/1
   ]).

all() ->
  [
   one_doc,
   target_not_empty,
   deleted_doc,
   over_http
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
  Config.

init_per_testcase(_, Config) ->
  ok = barrel_db:start(<<"testdb">>, barrel_test_rocksdb),
  ok = barrel_db:start(<<"source">>, barrel_test_rocksdb),
  Config.

end_per_testcase(_, _Config) ->
  ok = barrel_db:clean(<<"testdb">>),
  ok = barrel_db:clean(<<"source">>),
  ok.

end_per_suite(Config) ->
  %% TODO this gives an error
  %% {error_db_destroy,
  %%     "IO error: lock testdb/LOCK: No locks available"}}}

  %% ok = erocksdb:destroy("testdb", []),
  %% ok = erocksdb:destroy("source", []),
  Config.

one_doc(_Config) ->
  {ok, _Pid} = barrel_replicate:start_link(<<"source">>, <<"testdb">>),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(<<"source">>, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  timer:sleep(200),
  {ok, Doc2} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  stopped = barrel_replicate:stop(),
  ok.

target_not_empty(_Config) ->
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(<<"source">>, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},

  {ok, _Pid} = barrel_replicate:start_link(<<"source">>, <<"testdb">>),
  timer:sleep(200),

  {ok, Doc2} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  stopped = barrel_replicate:stop(),
  ok.

deleted_doc(_Config) ->
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(<<"source">>, <<"a">>, Doc, []),
  
  {ok, _Pid} = barrel_replicate:start_link(<<"source">>, <<"testdb">>),
  barrel_db:delete(<<"source">>, <<"a">>, RevId, []),
  timer:sleep(200),
  {ok, Doc3} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  true = maps:get(<<"_deleted">>, Doc3),
  stopped = barrel_replicate:stop(),
  ok.

over_http(_Config) -> 
  Source = <<"http://localhost:8080/source">>,
  {ok, _} = barrel_httpc:start_link(),
  ok = barrel_httpc:start(Source, undefined),
  Target= <<"testdb">>,
  {ok, _Pid} = barrel_replicate:start_link(Source, Target),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(<<"source">>, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  timer:sleep(200),
  {ok, Doc2} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  stopped = barrel_replicate:stop(),
  stopped = barrel_httpc:stop(),
  ok.
