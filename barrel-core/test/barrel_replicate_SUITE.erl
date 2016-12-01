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
  {true, Source} = barrel:create_database(barrel_source_rocksdb, source()),
  {true, Target} = barrel:create_database(barrel_source_rocksdb, target()),
  [{source, Source}, {target, Target} |Config].

end_per_testcase(_, Config) ->
  {Source, Target} = repctx(Config),
  ok = barrel_replicate:clean(Source, Target),
  ok = barrel:delete_database(Source),
  ok = barrel:delete_database(Target),
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

repctx(Config) ->
  {proplists:get_value(source, Config), proplists:get_value(target, Config)}.

%% =============================================================================
%% Basic usage
%% =============================================================================

one_doc(Config) ->
  {Source, Target} = repctx(Config),
  Options = [{metrics_freq, 100}],
  {ok, RepId} = barrel:start_replication(Source, Target, Options),
  %% Info = barrel_replicate:info(Pid),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(Source, <<"a">>, Doc, []),
  timer:sleep(200),
  {ok, Doc2} = barrel_db:get(Source, <<"a">>, []),
  {ok, Doc2} = barrel_db:get(Target, <<"a">>, []),
  ok = barrel:stop_replication(RepId),

  %%[Stats] = barrel_task_status:all(),
  %%1 = proplists:get_value(docs_read, Stats),
  %%1 = proplists:get_value(docs_written, Stats),

  {ok, _, _} = delete_doc("a", Source),
  {ok, _, _} = delete_doc("a", Target),
  %% Id = maps:get(id, Info),
  %% {ok, _Checkpoint} = barrel_db:get(source(), <<"_replication_", Id/binary>>, []),
  %% {ok, _Checkpoint} = barrel_db:get(target(), <<"_replication_", Id/binary>>, []),
  ok.

source_not_empty(Config) ->
  {Source, Target} = repctx(Config),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(Source, <<"a">>, Doc, []),
  {ok, Doc2} = barrel_db:get(Source, <<"a">>, []),
  {ok, RepId} = barrel:start_replication(Source, Target),
  timer:sleep(200),
  {ok, Doc2} = barrel_db:get(Target, <<"a">>, []),
  ok = barrel:stop_replication(RepId),
  ok.

deleted_doc(Config) ->
  {Source, Target} = repctx(Config),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(Source, <<"a">>, Doc, []),

  {ok, RepId} = barrel:start_replication(Source, Target),
  timer:sleep(200),
  {ok, #{ <<"id">> := <<"a">>, <<"_rev">> := RevId }} = barrel_db:get(Target, <<"a">>, []),
  barrel_db:delete(Source, <<"a">>, RevId, []),
  timer:sleep(400),
  {error, not_found} = barrel_db:get(Target, <<"a">>, []),
  ok = barrel:stop_replication(RepId),
  ok.

%% =============================================================================
%% Complex scenarios
%% =============================================================================

random_activity(Config) ->
  {Source, Target} = repctx(Config),
  Scenario = scenario(),
  {ok, RepId} = barrel:start_replication(Source, Target),
  ExpectedResults = play_scenario(Scenario, Source),
  timer:sleep(200),
  ok = check_all(ExpectedResults, Source, Target),
  ok = barrel:stop_replication(RepId),
  ok = purge_scenario(ExpectedResults, Source),
  ok = purge_scenario(ExpectedResults, Target),
  ok.

%% =============================================================================
%% Checkpoints
%% =============================================================================

checkpoints(Config) ->
  {Source, Target} = repctx(Config),
  Scenario = scenario(),
  [P1,P2,P3,P4] = split(4, Scenario),

  %% start and stop replication 4 times
  M1 = play_checkpoint(P1, maps:new(), Config),
  M2 = play_checkpoint(P2, M1, Config),
  M3 = play_checkpoint(P3, M2, Config),
  M4 = play_checkpoint(P4, M3, Config),

  Info = get_checkpoints(Source, Target),
  SourceCheckpoints = maps:get(source_checkpoints, Info),
  History = maps:get(<<"history">>, SourceCheckpoints),
  4 = length(History),
  LastSession = hd(History),
  12 = maps:get(<<"source_last_seq">>, LastSession),

  ok = purge_scenario(M4, Source),
  ok = purge_scenario(M2, Target),
  ok.

play_checkpoint(Scenario, M, Config) ->
  {Source, Target} = repctx(Config),
  {ok, RepId} = barrel:start_replication(Source, Target),
  Expected = play_scenario(Scenario, Source, M),
  timer:sleep(200),
  ok = check_all(Expected, Source, Target),
  ok = barrel:stop_replication(RepId),
  Expected.

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
  case maps:get(DocName, Map) of
    deleted ->
      {error, not_found} = barrel_db:get(Db1, Id, []),
      {error, not_found} = barrel_db:get(Db2, Id, []);
    Expected ->
      {ok, DocSource} = barrel_db:get(Db1, Id, []),
      {ok, DocTarget} = barrel_db:get(Db2, Id, []),
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
      Doc = #{<<"id">> => Id, <<"v">> => Value},
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

get_checkpoints(Source, Target) ->
  RepId = barrel_replicate:repid(Source, Target),
  {ok, SourceCheckpoint} = read_checkpoint_doc(Source, RepId),
  {ok, TargetCheckpoint} = read_checkpoint_doc(Target, RepId),
  #{id => RepId,
    source_checkpoints => SourceCheckpoint,
    target_checkpoints => TargetCheckpoint}.

read_checkpoint_doc(Db, RepId) ->
  barrel_db:read_system_doc(Db, checkpoint_docid(RepId)).

checkpoint_docid(RepId) ->
  <<"replication-checkpoint-", RepId/binary>>.
