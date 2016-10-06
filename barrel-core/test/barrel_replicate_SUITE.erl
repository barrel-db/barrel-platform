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
   [ all/0
   , init_per_suite/1
   , end_per_suite/1
   , init_per_testcase/2
   , end_per_testcase/2
   ]).

-export(
   [ one_doc/1
   , source_not_empty/1
   , deleted_doc/1
   , random_activity/1
   , checkpoints/1
   ]).

all() ->
  [ one_doc
  , source_not_empty
  , deleted_doc
  , random_activity
  , checkpoints
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
  Config.

init_per_testcase(_, Config) ->
  ok = barrel_db:start(source(), barrel_test_rocksdb),
  ok = barrel_db:start(target(), barrel_test_rocksdb),
  Config.

end_per_testcase(_, _Config) ->
  ok = barrel_db:clean(source()),
  ok = barrel_db:clean(target()),
  ok = barrel_replicate:clean(source(), target()),
  ok.

end_per_suite(Config) ->
  %% TODO this gives an error
  %% {error_db_destroy,
  %%     "IO error: lock testdb/LOCK: No locks available"}}}

  %% ok = erocksdb:destroy("testdb", []),
  %% ok = erocksdb:destroy("source", []),
  Config.


source() ->
  <<"source">>.

target() ->
  <<"testdb">>.

%% =============================================================================
%% Basic usage
%% =============================================================================

one_doc(_Config) ->
  Options = [{metrics_freq, 100}],
  {ok, Pid} = barrel_replicate:start_link(source(), target(), Options),
  %% Info = barrel_replicate:info(Pid),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(source(), <<"a">>, Doc, []),
  timer:sleep(200),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_db:get(target(), <<"a">>, []),
  stopped = barrel_replicate:stop(Pid),

  [Stats] = barrel_task_status:all(),
  1 = proplists:get_value(docs_read, Stats),
  1 = proplists:get_value(docs_written, Stats),

  {ok, _, _} = delete_doc("a", source()),
  {ok, _, _} = delete_doc("a", target()),
  %% Id = maps:get(id, Info),
  %% {ok, _Checkpoint} = barrel_db:get(source(), <<"_replication_", Id/binary>>, []),
  %% {ok, _Checkpoint} = barrel_db:get(target(), <<"_replication_", Id/binary>>, []),
  ok.

source_not_empty(_Config) ->
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(source(), <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},

  {ok, Pid} = barrel_replicate:start_link(source(), target()),
  timer:sleep(200),

  {ok, Doc2} = barrel_db:get(target(), <<"a">>, []),
  stopped = barrel_replicate:stop(Pid),
  ok.

deleted_doc(_Config) ->
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(source(), <<"a">>, Doc, []),

  {ok, Pid} = barrel_replicate:start_link(source(), target()),
  barrel_db:delete(source(), <<"a">>, RevId, []),
  timer:sleep(200),
  {ok,Doc3} = barrel_db:get(target(), <<"a">>, []),
  true = maps:get(<<"_deleted">>, Doc3),
  stopped = barrel_replicate:stop(Pid),
  ok.

%% =============================================================================
%% Complex scenarios
%% =============================================================================

random_activity(_Config) ->
  Scenario = scenario(),
  {ok, Pid} = barrel_replicate:start_link(source(), target()),
  ExpectedResults = play_scenario(Scenario, source()),
  timer:sleep(200),
  ok = check_all(ExpectedResults, source(), target()),
  stopped = barrel_replicate:stop(Pid),
  ok = purge_scenario(ExpectedResults, source()),
  ok = purge_scenario(ExpectedResults, target()),
  ok.

%% =============================================================================
%% Checkpoints
%% =============================================================================

checkpoints(_Config) ->
  Scenario = scenario(),
  [P1,P2,P3,P4] = split(4, Scenario),

  %% start and stop replication 4 times
  {_, M1} = play_checkpoint(P1, maps:new()),
  {_, M2} = play_checkpoint(P2, M1),
  {_, M3} = play_checkpoint(P3, M2),
  {Info, M4} = play_checkpoint(P4, M3),

  History = maps:get(checkpoints, Info),
  15 = length(History),

  ok = purge_scenario(M4, source()),
  ok = purge_scenario(M2, target()),
  ok.

play_checkpoint(Scenario, M) ->
  {ok, Pid} = barrel_replicate:start_link(source(), target()),
  Expected = play_scenario(Scenario, source(), M),
  timer:sleep(200),
  ok = check_all(Expected, source(), target()),
  Info = barrel_replicate:info(Pid),
  stopped = barrel_replicate:stop(Pid),
  {Info, Expected}.

split(2, L) ->
  {L1,L2} = lists:split(2, L),
  [L1,L2];
split(N, L) ->
  [L1, L2] = split(N div 2,L),
  S1 = split(N div 2, L1),
  S2 = split(N div 2, L2),
  S1 ++ S2.

%% =============================================================================
%% Scenario helpers
%% =============================================================================

play_scenario(Scenario, Db) ->
  play_scenario(Scenario, Db, maps:new()).

play_scenario(Scenario, Db, Map) ->
  lists:foldl(fun(C, Acc) ->
                  play(C, Db, Acc)
              end, Map, Scenario).

play({put, DocName, Value}, Db, Map)->
  put_doc(DocName, Value, Db),
  Map#{DocName => Value};
play({del, DocName}, Db, Map) ->
  delete_doc(DocName, Db),
  Map#{DocName => deleted}.

check_all(Map, Db1, Db2) ->
  Keys = maps:keys(Map),
  [ ok = check(K, Map, Db1, Db2) || K <- Keys ],
  ok.

check(DocName, Map, Db1, Db2) ->
  Id = list_to_binary(DocName),
  {ok, DocSource} = barrel_db:get(Db1, Id, []),
  {ok, DocTarget} = barrel_db:get(Db2, Id, []),
  case maps:get(DocName, Map) of
    deleted ->
      true = maps:get(<<"_deleted">>, DocSource),
      true = maps:get(<<"_deleted">>, DocTarget);
    Expected ->
      Expected = maps:get(<<"v">>, DocSource),
      Expected = maps:get(<<"v">>, DocTarget)
  end,
  ok.

purge_scenario(Map, Db) ->
  Keys = maps:keys(Map),
  [{ok, _,_} = delete_doc(K, Db) || K <- Keys],
  ok.

put_doc(DocName, Value, Db) ->
  Id = list_to_binary(DocName),
  case barrel_db:get(Db, Id, []) of
    {ok, Doc} ->
      Doc2 = Doc#{<<"v">> => Value},
      {ok,_,_} = barrel_db:put(Db, Id, Doc2, []);
    {error, not_found} ->
      Doc = #{<<"_id">> => Id, <<"v">> => Value},
      {ok,_,_} = barrel_db:put(Db, Id, Doc, [])
  end.

delete_doc(DocName, Db) ->
  Id = list_to_binary(DocName),
  {ok, Doc} = barrel_db:get(Db, Id, []),
  RevId = maps:get(<<"_rev">>, Doc),
  barrel_db:delete(Db, Id, RevId, []).

scenario() ->
  [ {put, "a", 1}
  , {put, "b", 1}
  , {put, "c", 1}
  , {put, "d", 1}
  , {put, "a", 2}
  , {put, "e", 1}
  , {put, "f", 1}
  , {put, "f", 2}
  , {del, "a"}
  , {put, "f", 3}
  , {put, "g", 1}
  , {put, "f", 4}
  ].
