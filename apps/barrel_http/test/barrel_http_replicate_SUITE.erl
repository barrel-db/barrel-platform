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
   , attachments/1
   , random_activity/1
   ]).

all() ->
  [ one_doc
  , target_not_empty
  , deleted_doc
  , attachments
  , random_activity
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel_store),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  _ = barrel_store:create_db(<<"source">>, #{}),
  [{source_conn, source()},{target_conn, target()},
   {source, <<"source">>}, {target, <<"testdb">>} | Config].

end_per_testcase(_, _Config) ->
  ok = barrel_local:delete_db(<<"testdb">>),
  ok = barrel_local:delete_db(<<"source">>),
  ok.

end_per_suite(Config) ->
  ok = application:stop(barrel_http),
  ok = application:stop(barrel_replicate),
  ok = application:stop(barrel_store),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


source_url() ->
  <<"http://localhost:7080/dbs/source">>.

source() ->
  {barrel_httpc, source_url()}.

target_url() ->
  <<"http://localhost:7080/dbs/testdb">>.

target() ->
  {barrel_httpc, target_url()}.

repctx(Config) ->
  {proplists:get_value(source, Config), proplists:get_value(target, Config)}.


one_doc(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  RepConfig = #{<<"source">> => SourceConn,
                <<"target">> => TargetConn},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, []),
  %% {ok, Pid} = barrel_replicate:start_replication(SourceConn, TargetConn, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:post(Source, Doc, []),
  {1, [_]} = source_changes(0),
  {ok, _, _} = barrel_local:get(Source, <<"a">>, []),
  timer:sleep(200),
  {ok, Doc, #{ <<"rev">> := RevId}} = barrel_local:get(Target, <<"a">>, []),
  ok = barrel_replicate:stop_replication(RepId),
  ok.

source_changes(Since) ->
  Fun = fun(Change, {PreviousLastSeq, Changes1}) ->
            Seq  = maps:get(<<"seq">>, Change),
            LastSeq = max(Seq, PreviousLastSeq),
            {ok, {LastSeq, [Change|Changes1]}}
        end,
  barrel_local:changes_since(<<"source">>, Since, Fun, {Since, []}).

target_not_empty(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  Doc = #{ <<"id">> => <<"targetnotempty">>, <<"v">> => 1},
  {ok, <<"targetnotempty">>, RevId} = barrel_local:post(Source, Doc, []),

  RepConfig = #{<<"source">> => SourceConn,
                <<"target">> => TargetConn},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, []),
  timer:sleep(200),

  {ok, Doc, #{ <<"rev">> := RevId}} = barrel_local:get(Target, <<"targetnotempty">>, []),
  ok = barrel_local:stop_replication(RepId),
  ok.

deleted_doc(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  DocId = <<"tobedeleted">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(Source, Doc, []),

  RepConfig = #{<<"source">> => SourceConn,
                <<"target">> => TargetConn},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, []),
  {ok, _, _} = barrel_local:delete(Source, DocId, [{rev, RevId}]),
  {error, not_found} = barrel_local:get(Source, DocId, []),
  timer:sleep(400),
  {error, not_found} = barrel_local:get(Target, DocId, []),
  ok = barrel_local:stop_replication(RepId),
  ok.

attachments(Config) ->
  {ok, HttpcSource} = barrel_httpc:connect(source_url()),
  {ok, HttpcTarget} = barrel_httpc:connect(target_url()),
  DocId = <<"withattachments">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  AttId = <<"myattachement">>,
  Blob = <<"blobdata">>,
  Attachments = [#{<<"id">> => AttId,
                   <<"blob">> => Blob}],
  {ok, DocId, _} = barrel_httpc:post(HttpcSource, Doc, Attachments, []),

  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  RepConfig = #{<<"source">> => SourceConn,
                <<"target">> => TargetConn},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, []),
  timer:sleep(400),

  {ok, Doc, [A], _} = barrel_httpc:get(HttpcTarget, DocId, [{attachments, all}]),
  #{<<"id">> := AttId,
    <<"blob">> := Blob} = A,

  ok = barrel_local:stop_replication(RepId),
  ok.

random_activity(Config) ->
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  Scenario = generate_scenario(),
  RepConfig = #{<<"source">> => SourceConn,
                <<"target">> => TargetConn},
  {ok, #{<<"replication_id">> := RepId}} =
    barrel_replicate:start_replication(RepConfig, []),
  ExpectedResults = play_scenario(Scenario, Config),
  timer:sleep(1000),
  ok = check_all(ExpectedResults, Config),
  ok = barrel_replicate:stop_replication(RepId),
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
  DocId = list_to_binary(DocName),
  case maps:get(DocName, Map) of
    deleted ->
      {error, not_found} = barrel_local:get(Source, DocId, []),
      {error, not_found} = barrel_local:get(Target, DocId, []);
    Expected ->
      {ok, DocSource, _} = barrel_local:get(Source, DocId, []),
      {ok, DocTarget, _} = barrel_local:get(Target, DocId, []),
      Expected = maps:get(<<"v">>, DocSource),
      Expected = maps:get(<<"v">>, DocTarget)
  end,
  ok.

put_doc(DocName, Value, Config) ->
  {Source, _Target} = repctx(Config),
  DocId = list_to_binary(DocName),
  case barrel_local:get(Source, DocId, []) of
    {ok, Doc, Meta} ->
      Doc2 = Doc#{<<"v">> => Value},
      {ok,_,_} = barrel_local:put(Source, Doc2, [{rev, maps:get(<<"rev">>, Meta)}]);
    {error, not_found} ->
      Doc = #{<<"id">> => DocId, <<"v">> => Value},
      {ok,_,_} = barrel_local:post(Source, Doc, [])
  end.

delete_doc(DocName, Config) ->
  {Source, _Target} = repctx(Config),
  Id = list_to_binary(DocName),
  {ok, _Doc, Meta} = barrel_local:get(Source, Id, []),
  RevId = maps:get(<<"rev">>, Meta),
  barrel_local:delete(Source, Id, [{rev, RevId}]).

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
  %% , {put, "a", 3}
  %% , {put, "f", 3}
  , {del, "f"}
  ].
