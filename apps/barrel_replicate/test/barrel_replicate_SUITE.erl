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
   , delete_database_being_replicated/1
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
   , delete_database_being_replicated
   , random_activity
   , checkpoints
   ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_store),
  {ok, _} = application:ensure_all_started(barrel_replicate),
  Config.

init_per_testcase(_, Config) ->
  {ok, _} = barrel_store:create_db(<<"testdb">>, #{}),
  {ok, _} = barrel_store:create_db(<<"source">>, #{}),
  [{db, <<"testdb">>} | Config].

end_per_testcase(_, _Config) ->
  ok = barrel_store:delete_db(<<"testdb">>),
  ok = barrel_store:delete_db(<<"source">>),
  ok.

end_per_suite(Config) ->
  ok = application:stop(barrel_replicate),
  ok = application:stop(barrel_store),
  Config.

%% =============================================================================
%% Basic usage
%% =============================================================================

one_doc(_Config) ->
  Options = [{metrics_freq, 100}],
  RepConfig = #{<<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  %% Info = barrel_replicate:info(Pid),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_local:put(<<"source">>, Doc, []),
  timer:sleep(200),
  {ok, Doc2} = barrel_local:get(<<"source">>, <<"a">>, []),
  {ok, Doc2} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  ok = barrel_replicate:stop_replication(RepId),

  %%[Stats] = barrel_task_status:all(),
  %%1 = proplists:get_value(docs_read, Stats),
  %%1 = proplists:get_value(docs_written, Stats),

  ok = delete_doc("a", <<"source">>),
  ok = delete_doc("a", <<"testdb">>),
  %% Id = maps:get(id, Info),
  %% {ok, _Checkpoint} = barrel:get(<<"source">>(), <<"_replication_", Id/binary>>, []),
  %% {ok, _Checkpoint} = barrel:get(target(), <<"_replication_", Id/binary>>, []),
  ok.

source_not_empty(_Config) ->
  RepId = <<"source_notempty">>,
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_local:put(<<"source">>, Doc, []),
  {ok, Doc2} = barrel_local:get(<<"source">>, <<"a">>, []),
  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, []),
  timer:sleep(200),
  {ok, Doc2} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  ok = barrel_replicate:stop_replication(RepId),
  ok.

deleted_doc(_Config) ->
  RepId = <<"deleteddoc">>,
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:put(<<"source">>, Doc, []),

  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, []),
  timer:sleep(200),
  {ok, #{ <<"id">> := <<"a">>, <<"_rev">> := RevId }} = barrel_local:get(<<"source">>, <<"a">>, []),
  {ok, _, _} = barrel_local:delete(<<"source">>, <<"a">>, RevId, []),
  timer:sleep(400),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  ok = barrel_replicate:stop_replication(RepId),
  ok.


persistent_replication(_Config) ->
  RepId = <<"a">>,
  Options = [{metrics_freq, 100}, {persist, true}],
  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  [{RepId, {true, Pid, _}}] = ets:lookup(replication_ids, RepId),
  true = is_pid(Pid),
  [{Pid, {true, RepId}}] = ets:lookup(replication_ids, Pid),
  ok = barrel_replicate:stop_replication(RepId),
  [{RepId, {true, nil, nil}}] = ets:lookup(replication_ids, RepId),
  [] = ets:lookup(replication_ids, Pid),
  {ok, [AllConfig2]} = file:consult("data/replication.config"),
  RepConfig2 = maps:get(<<"a">>, AllConfig2),
  #{ <<"source">> := <<"source">>, <<"target">> := <<"testdb">>, options := Options} = RepConfig2,
  ok = barrel_replicate:delete_replication(RepId),
  {ok, [AllConfig3]} = file:consult("data/replication.config"),
  undefined = maps:get(RepId, AllConfig3, undefined),
  [] = ets:lookup(replication_ids, RepId),
  ok.

restart_persistent_replication(_Config) ->
  RepId = <<"restart">>,
  Options = [{metrics_freq, 100}, {persist, true}],
  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  %% {ok, [AllConfig]} = file:consult("data/replication.config"),
  %% RepConfig = maps:get(RepId, AllConfig),
  Manager = whereis(barrel_replicate),
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
  {'EXIT', {badarg, _}} = (catch ets:lookup(replication_ids, RepId)),
  timer:sleep(200),
  ok = barrel_replicate:delete_replication(RepId),
  {ok, [AllConfig3]} = file:consult("data/replication.config"),
  undefined = maps:get(RepId, AllConfig3, undefined),
  [] = ets:lookup(replication_ids, RepId),
  ok.


start_duplicate_replication(_Config) ->
  RepId = <<"duplicate">>,
  Options = [{metrics_freq, 100}, {persist, true}],
  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  {error, {task_already_running, RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  ok = barrel_replicate:delete_replication(RepId),
  ok.


delete_database_being_replicated(_Config) ->
  {ok, _} = barrel_store:create_db(<<"tobedeleted">>, #{}),
  RepId = <<"sourcedatabasedeleted">>,
  Options = [],
  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"tobedeleted">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  Manager = whereis(barrel_replicate),
  [{<<"sourcedatabasedeleted">>, {false, _Pid, _}}] = ets:lookup(replication_ids, RepId),
  ok = barrel_store:delete_db(<<"tobedeleted">>),
  timer:sleep(200),
  Manager = whereis(barrel_replicate),
  [] = ets:lookup(replication_ids, RepId),
  ok = barrel_replicate:delete_replication(RepId),
  ok.

%% =============================================================================
%% Complex scenarios
%% =============================================================================

random_activity(_Config) ->
  Scenario = scenario(),
  RepId = <<"random">>,
  Options = [],
  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  ExpectedResults = play_scenario(Scenario, <<"source">>),
  timer:sleep(200),
  ok = check_all(ExpectedResults, <<"source">>, <<"testdb">>),
  ok = barrel_replicate:stop_replication(RepId),
  ok = purge_scenario(ExpectedResults, <<"source">>),
  ok = purge_scenario(ExpectedResults, <<"testdb">>),
  ok.

%% =============================================================================
%% Checkpoints
%% =============================================================================

checkpoints(_Config) ->
  Scenario = scenario(),
  [P1,P2,P3,P4] = split(4, Scenario),

  %% start and stop replication 4 times
  M1 = play_checkpoint(P1, maps:new()),
  M2 = play_checkpoint(P2, M1),
  M3 = play_checkpoint(P3, M2),
  M4 = play_checkpoint(P4, M3),

  SourceDbId = <<"source">>,
  RepId = <<"checkpoints">>,
  {ok, SourceCheckpoints} = read_checkpoint_doc(SourceDbId, RepId),
  History = maps:get(<<"history">>, SourceCheckpoints),
  4 = length(History),
  LastSession = hd(History),
  12 = maps:get(<<"source_last_seq">>, LastSession),

  ok = purge_scenario(M4, <<"source">>),
  ok = purge_scenario(M2, <<"testdb">>),
  ok.

play_checkpoint(Scenario, M) ->
  RepId = <<"checkpoints">>,
  Options = [],
  RepConfig = #{<<"replication_id">> => RepId,
                <<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, Options),
  Expected = play_scenario(Scenario, <<"source">>, M),
  timer:sleep(200),
  ok = check_all(Expected, <<"source">>, <<"testdb">>),
  ok = barrel_replicate:stop_replication(RepId),
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
      {error, not_found} = barrel_local:get(Db1, Id, []),
      {error, not_found} = barrel_local:get(Db2, Id, []);
    Expected ->
      {ok, DocSource} = barrel_local:get(Db1, Id, []),
      {ok, DocTarget} = barrel_local:get(Db2, Id, []),
      Expected = maps:get(<<"v">>, DocSource),
      Expected = maps:get(<<"v">>, DocTarget)
  end,
  ok.

purge_scenario(Map, Db) ->
  Keys = maps:keys(Map),
  [ok= delete_doc(K, Db) || K <- Keys],
  ok.

put_doc(DocName, Value, Db) ->
  Id = list_to_binary(DocName),
  case barrel_local:get(Db, Id, []) of
    {ok, Doc} ->
      Doc2 = Doc#{<<"v">> => Value},
      {ok,_,_} = barrel_local:put(Db, Doc2, []);
    {error, not_found} ->
      Doc = #{<<"id">> => Id, <<"v">> => Value},
      {ok,_,_} = barrel_local:put(Db, Doc, [])
  end.

delete_doc(DocName, Db) ->
  Id = list_to_binary(DocName),
  case barrel_local:get(Db, Id, []) of
    {error, not_found} -> ok;
    {ok, Doc} ->
      RevId = maps:get(<<"_rev">>, Doc),
      {ok, _, _} = barrel_local:delete(Db, Id, RevId, []),
      ok
  end.

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

read_checkpoint_doc(Db, RepId) ->
  barrel_db:get_system_doc(Db, checkpoint_docid(RepId)).

checkpoint_docid(RepId) ->
  <<"replication-checkpoint-", RepId/binary>>.
