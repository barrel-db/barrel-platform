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

-module(barrel_http_replicate_SUITE).
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
   , target_not_empty/1
   , deleted_doc/1
   , random_activity/1
   ]).

all() ->
  [ one_doc
  , target_not_empty
  %% , deleted_doc
  %% , random_activity
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  Config.

init_per_testcase(_, Config) ->
  {true, Target} = barrel:create_database(testdb, <<"testdb">>),
  {true, Source} = barrel:create_database(source, <<"source">>),
  [{source_conn, source()},{target_conn, target()}, {source, Source}, {target, Target}|Config].

end_per_testcase(_, Config) ->
  {Source, Target} = repctx(Config),
  ok = barrel:delete_database(Target),
  ok = barrel:delete_database(Source),
  ok.

end_per_suite(Config) ->
  %% TODO this gives an error
  %% {error_db_destroy,
  %%     "IO error: lock testdb/LOCK: No locks available"}}}

  %% ok = erocksdb:destroy("testdb", []),
  %% ok = erocksdb:destroy("source", []),
  Config.


repctx(Config) ->
  {proplists:get_value(source, Config), proplists:get_value(target, Config)}.


source_url() ->
  <<"http://localhost:8080/source/source">>.

source() ->
  {barrel_httpc, source_url()}.

target_url() ->
  <<"http://localhost:8080/testdb/testdb">>.

target() ->
  {barrel_httpc, target_url()}.

one_doc(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  {ok, Pid} = barrel:start_replication(SourceConn, TargetConn, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel:put(Source, <<"a">>, Doc, []),
  {1, [_]} = changes(Source, 0),
  {ok, _} = barrel:get(Source, <<"a">>, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  timer:sleep(200),
  {ok, Doc2} = barrel:get(Target, <<"a">>, []),
  ok = barrel:stop_replication(Pid),
  ok.

changes(Conn, Since) ->
  Fun = fun(Seq, Change, {PreviousLastSeq, Changes1}) ->
            LastSeq = max(Seq, PreviousLastSeq),
            {ok, {LastSeq, [Change|Changes1]}}
        end,
  barrel:changes_since(Conn, Since, Fun, {Since, []}).

target_not_empty(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel:put(Source, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},

  {ok, Pid} = barrel:start_replication(SourceConn, TargetConn, []),
  timer:sleep(200),

  {ok, Doc2} = barrel:get(Target, <<"a">>, []),
  ok = barrel:stop_replication(Pid),
  ok.

deleted_doc(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel:put(Source, <<"a">>, Doc, []),

  {ok, Pid} = barrel:start_replication(SourceConn, TargetConn, []),
  barrel:delete(Source, <<"a">>, RevId, []),
  timer:sleep(200),
  {ok, Doc3} = barrel:get(Target, <<"a">>, []),

  %% TODO bug to be fixed
  %% https://gitlab.com/barrel-db/barrel-http/issues/1
  true = maps:get(<<"_deleted">>, Doc3),

  ok = barrel:stop_replication(Pid),
  ok.

random_activity(Config) ->
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  Scenario = generate_scenario(),
  {ok, Pid} = barrel:start_replication(SourceConn, TargetConn, []),
  ExpectedResults = play_scenario(Scenario, Config),
  timer:sleep(200),
  ok = check_all(ExpectedResults, Config),
  ok = barrel:stop_replication(Pid),
  ok.

play_scenario(Scenario, Config) ->
  Map = maps:new(),
  lists:foldl(fun(C, Acc) ->
                  play(C, Acc, Config)
              end, Map, Scenario).

play({put, DocName, Value}, Map, Config)->
  put_doc(DocName,Value, Config),
  Map#{DocName => Value};
play({del, DocName}, Map, Config) ->
  delete_doc(DocName, Config),
  Map#{DocName => deleted}.

check_all(Map, Config) ->
 Keys = maps:keys(Map),
  [ ok = check(K, Map, Config) || K <- Keys ],
  ok.

check(DocName, Map, Config) ->
  {Source, Target} =  repctx(Config),
  Id = list_to_binary(DocName),
  {ok, DocSource} = barrel:get(Source, Id, []),
  {ok, DocTarget} = barrel:get(Target, Id, []),
  case maps:get(DocName, Map) of
    deleted ->
      true = maps:get(<<"_deleted">>, DocSource),
      true = maps:get(<<"_deleted">>, DocTarget);
    Expected ->
      Expected = maps:get(<<"v">>, DocSource),
      Expected = maps:get(<<"v">>, DocTarget)
  end,
  ok.

put_doc(DocName, Value, Config) ->
  {Source, _Target} = repctx(Config),
  Id = list_to_binary(DocName),
  case barrel:get(Source, Id, []) of
    {ok, Doc} ->
      Doc2 = Doc#{<<"v">> => Value},
      {ok,_,_} = barrel:put(Source, Id, Doc2, []);
    {error, not_found} ->
      Doc = #{<<"id">> => Id, <<"v">> => Value},
      {ok,_,_} = barrel:put(Source, Id, Doc, [])
  end.

delete_doc(DocName, Config) ->
  {Source, _Target} = repctx(Config),
  Id = list_to_binary(DocName),
  {ok, Doc} = barrel:get(Source, Id, []),
  RevId = maps:get(<<"_rev">>, Doc),
  barrel:delete(Source, Id, RevId, []).

generate_scenario() ->
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
  ].
