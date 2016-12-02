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
   , persistent_replication/1
   , restart_persistent_replication/1
   , start_duplicate_replication/1
   , start_replication_error/1
   , random_activity/1
   , checkpoints/1
   ]).

all() ->
  [ one_doc
  , source_not_empty
  , deleted_doc
  , persistent_replication
  , restart_persistent_replication
  , start_duplicate_replication
  , start_replication_error
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
  Name = <<"onedoc">>,
  Options = [{metrics_freq, 100}],
  {ok, Name} = barrel:start_replication(Name, Source, Target, Options),
  %% Info = barrel_replicate:info(Pid),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(Source, <<"a">>, Doc, []),
  timer:sleep(200),
  {ok, Doc2} = barrel_db:get(Source, <<"a">>, []),
  {ok, Doc2} = barrel_db:get(Target, <<"a">>, []),
  ok = barrel:stop_replication(Name),

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
  Name = <<"sourcenotempty">>,
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(Source, <<"a">>, Doc, []),
  {ok, Doc2} = barrel_db:get(Source, <<"a">>, []),
  {ok, Name} = barrel:start_replication(Name, Source, Target),
  timer:sleep(200),
  {ok, Doc2} = barrel_db:get(Target, <<"a">>, []),
  ok = barrel:stop_replication(Name),
  ok.

deleted_doc(Config) ->
  {Source, Target} = repctx(Config),
  Name = <<"deleteddoc">>,
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(Source, <<"a">>, Doc, []),

  {ok, Name} = barrel:start_replication(Name, Source, Target),
  barrel_db:delete(Source, <<"a">>, RevId, []),
  timer:sleep(200),
  {ok,Doc3} = barrel_db:get(Target, <<"a">>, []),
  true = maps:get(<<"_deleted">>, Doc3),
  ok = barrel:stop_replication(Name),
  ok.


persistent_replication(Config) ->
  {Source, Target} = repctx(Config),
  Name = <<"a">>,
  Options = [{metrics_freq, 100}, {persist, true}],
  {ok, Name} = barrel:start_replication(Name, Source, Target, Options),
  {ok, [AllConfig]} = file:consult("data/replication.config"),
  RepConfig = maps:get(<<"a">>, AllConfig),
  #{ source := Source, target := Target, options := Options} = RepConfig,
  RepId = barrel_replicate:repid(Source, Target),
  [{<<"a">>, {RepId, Pid, true}}] = ets:lookup(replication_names, <<"a">>),
  true = is_pid(Pid),
  [{RepId, {Name, true, Pid, _}}] = ets:lookup(replication_ids, RepId),
  [{Pid, {Name, true, RepId}}] = ets:lookup(replication_ids, Pid),
  ok = barrel:stop_replication(<<"a">>),
  [{<<"a">>, {RepId, nil, true}}] = ets:lookup(replication_names, <<"a">>),
  [{RepId, {Name, true, nil, nil}}] = ets:lookup(replication_ids, RepId),
  [] = ets:lookup(replication_ids, Pid),
  {ok, [AllConfig2]} = file:consult("data/replication.config"),
  RepConfig2 = maps:get(<<"a">>, AllConfig2),
  #{ source := Source, target := Target, options := Options} = RepConfig2,
  ok = barrel:delete_replication(<<"a">>),
  {ok, [AllConfig3]} = file:consult("data/replication.config"),
  undefined = maps:get(<<"a">>, AllConfig3, undefined),
  [] = ets:lookup(replication_names, <<"a">>),
  [] = ets:lookup(replication_ids, RepId),
  ok.

restart_persistent_replication(Config) ->
  {Source, Target} = repctx(Config),
  Name = <<"a">>,
  Options = [{metrics_freq, 100}, {persist, true}],
  {ok, Name} = barrel:start_replication(Name, Source, Target, Options),
  {ok, [AllConfig]} = file:consult("data/replication.config"),
  RepConfig = maps:get(<<"a">>, AllConfig),
  #{ source := Source, target := Target, options := Options} = RepConfig,
  Manager = whereis(barrel_replicate_manager),
  MRef = erlang:monitor(process, Manager),
  try
    catch unlink(Manager),
    catch exit(Manager, shutdown),
    receive
      {'DOWN', MRef, _, _, _} -> ok
    end
  after
    erlang:demonitor(MRef, [flush])
  end,
  {'EXIT', {badarg, _}} = (catch ets:lookup(replication_names, <<"a">>)),
  timer:sleep(200),
  RepId = barrel_replicate:repid(Source, Target),
  [{<<"a">>, {RepId, Pid, true}}] = ets:lookup(replication_names, <<"a">>),
  true = is_pid(Pid),
  ok = barrel:delete_replication(<<"a">>),
  {ok, [AllConfig3]} = file:consult("data/replication.config"),
  undefined = maps:get(<<"a">>, AllConfig3, undefined),
  [] = ets:lookup(replication_names, <<"a">>),
  [] = ets:lookup(replication_ids, RepId),
  ok.


start_duplicate_replication(Config) ->
  {Source, Target} = repctx(Config),
  Name = <<"a">>,
  Options = [{metrics_freq, 100}, {persist, true}],
  {ok, Name} = barrel:start_replication(Name, Source, Target, Options),
  {ok, Name} = barrel:start_replication(Name, Source, Target, Options),
  ok = barrel:delete_replication(<<"a">>),
  ok.

start_replication_error(Config) ->
  {Source, Target} = repctx(Config),
  Name = <<"a">>,
  Options = [{metrics_freq, 100}, {persist, true}],
  {ok, Name} = barrel:start_replication(Name, Source, Target, Options),
  {error, {task_already_registered, <<"a">>}} = barrel:start_replication(<<"b">>, Source, Target, Options),
  ok = barrel:stop_replication(<<"a">>),
  {error, {task_already_registered, <<"a">>}} = barrel:start_replication(<<"b">>, Source, Target, Options),
  ok = barrel:delete_replication(<<"a">>),
  ok.


%% =============================================================================
%% Complex scenarios
%% =============================================================================

random_activity(Config) ->
  {Source, Target} = repctx(Config),
  Name = <<"random">>,
  Scenario = scenario(),
  {ok, Name} = barrel:start_replication(Name, Source, Target),
  ExpectedResults = play_scenario(Scenario, Source),
  timer:sleep(200),
  ok = check_all(ExpectedResults, Source, Target),
  ok = barrel:stop_replication(Name),
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
  Name = <<"checkpoints">>,
  {ok, Name} = barrel:start_replication(Name, Source, Target),
  Expected = play_scenario(Scenario, Source, M),
  timer:sleep(200),
  ok = check_all(Expected, Source, Target),
  ok = barrel:stop_replication(Name),
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
