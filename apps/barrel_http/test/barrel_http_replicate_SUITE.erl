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
  , deleted_doc
  , random_activity
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  _ = barrel_store:create_db(<<"source">>, #{}),
  [{source_conn, source()},{target_conn, target()}, {source, <<"source">>}, {target, <<"testdb">>}|Config].

end_per_testcase(_, _Config) ->
  ok = barrel:delete_db(<<"testdb">>),
  ok = barrel:delete_db(<<"source">>),
  ok.

end_per_suite(Config) ->
  %%application:stop(barrel_http),
  application:stop(barrel),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


source_url() ->
  <<"http://localhost:8080/source">>.

source() ->
  {barrel_httpc, source_url()}.

target_url() ->
  <<"http://localhost:8080/testdb">>.

target() ->
  {barrel_httpc, target_url()}.

repctx(Config) ->
  {proplists:get_value(source, Config), proplists:get_value(target, Config)}.


one_doc(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  {ok, Pid} = barrel_replicate:start_replication(SourceConn, TargetConn, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel:put(Source, Doc, []),
  {1, [_]} = changes(Source, 0),
  {ok, _} = barrel:get(Source, <<"a">>, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  timer:sleep(200),
  {ok, Doc2} = barrel:get(Target, <<"a">>, []),
  ok = barrel_replicate:stop_replication(Pid),
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
  Doc = #{ <<"id">> => <<"targetnotempty">>, <<"v">> => 1},
  {ok, <<"targetnotempty">>, RevId} = barrel:put(Source, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},

  {ok, Pid} = barrel_replicate:start_replication(SourceConn, TargetConn, []),
  timer:sleep(200),

  {ok, Doc2} = barrel:get(Target, <<"targetnotempty">>, []),
  ok = barrel:stop_replication(Pid),
  ok.

deleted_doc(Config) ->
  {Source, Target} = repctx(Config),
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  DocId = <<"tobedeleted">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, DocId, RevId} = barrel:put(Source, Doc, []),

  {ok, Pid} = barrel_replicate:start_replication(SourceConn, TargetConn, []),
  barrel:delete(Source, DocId, RevId, []),
  timer:sleep(200),
  {error, not_found} = barrel:get(Target, DocId, []),
  ok = barrel:stop_replication(Pid),
  ok.

random_activity(Config) ->
  SourceConn = proplists:get_value(source_conn, Config),
  TargetConn = proplists:get_value(target_conn, Config),
  Scenario = generate_scenario(),
  {ok, Pid} = barrel_replicate:start_replication(SourceConn, TargetConn, []),
  ExpectedResults = play_scenario(Scenario, Config),
  timer:sleep(1000),
  ok = check_all(ExpectedResults, Config),
  ok = barrel_replicate:stop_replication(Pid),
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
      {error, not_found} = barrel:get(Source, DocId, []),
      {error, not_found} = barrel:get(Target, DocId, []);
    Expected ->
      {ok, DocSource} = barrel:get(Source, DocId, []),
      {ok, DocTarget} = barrel:get(Target, DocId, []),
      Expected = maps:get(<<"v">>, DocSource),
      Expected = maps:get(<<"v">>, DocTarget)
  end,
  ok.

put_doc(DocName, Value, Config) ->
  {Source, _Target} = repctx(Config),
  DocId = list_to_binary(DocName),
  case barrel:get(Source, DocId, []) of
    {ok, Doc} ->
      Doc2 = Doc#{<<"v">> => Value},
      {ok,_,_} = barrel:put(Source, Doc2, []);
    {error, not_found} ->
      Doc = #{<<"id">> => DocId, <<"v">> => Value},
      {ok,_,_} = barrel:put(Source, Doc, [])
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
  %% , {put, "a", 3}
  %% , {put, "f", 3}
  , {del, "f"}
  ].
