%% Copyright 2016, Benoit Chesneau
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

%% Created by benoitc on 03/09/16.

-module(barrel_db_SUITE).
-author("Benoit Chesneau").

%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([
  basic_op/1,
  update_doc/1,
  async_update/1,
  revision_conflict/1,
  bad_doc/1,
  create_doc/1,
  docs_count/1,
  fold_by_id/1,
  change_since/1,
  change_since_many/1,
  change_since_include_doc/1,
  revsdiff/1,
  get_revisions/1,
  put_rev/1,
  change_deleted/1,
  resource_id/1
]).

all() ->
  [
    basic_op,
    update_doc,
    async_update,
    revision_conflict,
    bad_doc,
    create_doc,
    docs_count,
    get_revisions,
    fold_by_id,
    change_since,
    change_since_many,
    change_since_include_doc,
    revsdiff,
    put_rev,
    change_deleted,
    resource_id
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_store),
  Config.

init_per_testcase(_, Config) ->
  {ok, _} = barrel_store:create_db(<<"testdb">>, #{}),
  [{db, <<"testdb">>} | Config].

end_per_testcase(_, _Config) ->
  ok = barrel_store:delete_db(<<"testdb">>),
  timer:sleep(200),
  ok.

end_per_suite(Config) ->
  application:stop(barrel_store),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


basic_op(_Config) ->
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_local:delete(<<"testdb">>, <<"a">>, RevId, []),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"a">>, []).

update_doc(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  Doc3 = Doc2#{ v => 2},
  {ok, <<"a">>, RevId2} = barrel_local:put(<<"testdb">>, Doc3, []),
  true = (RevId =/= RevId2),
  Doc4 = Doc3#{<<"_rev">> => RevId2},
  {ok, Doc4} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_local:delete(<<"testdb">>, <<"a">>, RevId2, []),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  {ok, <<"a">>, _RevId3} = barrel_local:put(<<"testdb">>, Doc, []).


revision_conflict(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, _, _} = barrel_local:put(<<"testdb">>, Doc, []),
  {ok, Doc1} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  Doc2 = Doc1#{ <<"v">> => 2 },
  {ok, <<"a">>, _RevId} = barrel_local:put(<<"testdb">>, Doc2, []),
  {conflict, revision_conflict} = barrel_local:put(<<"testdb">>, Doc2, []),
  ok.


async_update(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  ok= barrel_local:put(<<"testdb">>, Doc, [{async, true}]),
  timer:sleep(100),
  {ok, #{ <<"id">> := <<"a">>, <<"v">> := 1}} = barrel_local:get(<<"testdb">>, <<"a">>, []).

resource_id(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  {ok, #{ <<"_rev">> := RevId, <<"_rid">> := Rid}} = barrel_local:get(<<"testdb">>, <<"a">>, [{meta, true}]),
  1 = barrel_db:decode_rid(Rid),
  {ok, <<"a">>, RevId2} = barrel_local:put(<<"testdb">>, Doc#{ <<"_rev">> => RevId}, []),
  {ok, #{ <<"_rev">> := RevId2, <<"_rid">> := Rid}} = barrel_local:get(<<"testdb">>, <<"a">>, [{meta, true}]),
  Doc2 = #{ <<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _} = barrel_local:put(<<"testdb">>, Doc2, []),
  {ok, #{ <<"_rid">> := Rid2}} = barrel_local:get(<<"testdb">>, <<"b">>, [{meta, true}]),
  2 = barrel_db:decode_rid(Rid2).

bad_doc(_Config) ->
  Doc = #{ <<"v">> => 1},
  try barrel_local:put(<<"testdb">>, Doc, [])
  catch
    error:{bad_doc, invalid_docid} -> ok
  end.

create_doc(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  CreatedDoc = Doc#{ <<"id">> => DocId, <<"_rev">> => RevId},
  {ok, CreatedDoc} = barrel_local:get(<<"testdb">>, DocId, []),
  {error, not_found} = barrel_local:post(<<"testdb">>, CreatedDoc, []),
  Doc2 = #{<<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_local:post(<<"testdb">>, Doc2, []).

docs_count(_Config) ->
  #{ docs_count := 0 } = barrel_local:db_infos(<<"testdb">>),
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  #{ docs_count := 1 } = barrel_local:db_infos(<<"testdb">>),
  {ok, _, _} = barrel_local:delete(<<"testdb">>, DocId, RevId, []),
  #{ docs_count := 0 } = barrel_local:db_infos(<<"testdb">>).

get_revisions(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc2} = barrel_local:get(<<"testdb">>, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_local:put(<<"testdb">>, Doc3, []),
  {ok, Doc4} = barrel_local:get(<<"testdb">>, DocId, [{history, true}]),
  Revisions = barrel_doc:parse_revisions(Doc4),
  Revisions == [RevId2, RevId].

put_rev(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc2} = barrel_local:get(<<"testdb">>, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_local:put(<<"testdb">>, Doc3, []),
  Doc4_0 = Doc2#{ v => 3 },
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc4_0),
  Doc4 = Doc4_0#{<<"_rev">> => NewRev},
  History = [NewRev, RevId],
  {ok, DocId, _RevId3} = barrel_local:put_rev(<<"testdb">>, Doc4, History, []),
  {ok, Doc5} = barrel_local:get(<<"testdb">>, DocId, [{history, true}]),
  Revisions = barrel_doc:parse_revisions(Doc5),
  Revisions == [RevId2, RevId].


fold_by_id(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  Doc2 = #{ <<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_local:put(<<"testdb">>, Doc2, []),
  Doc3 = #{ <<"id">> => <<"c">>, <<"v">> => 1},
  {ok, <<"c">>, _RevId3} = barrel_local:put(<<"testdb">>, Doc3, []),
  Fun = fun(DocId, _DocInfo, {ok, FoldDoc}, Acc1) ->
      DocId = barrel_doc:id(FoldDoc),
      {ok, [DocId | Acc1]}
    end,
  Acc = barrel_local:fold_by_id(<<"testdb">>, Fun, [], [{include_doc, true}]),
  [<<"c">>, <<"b">>, <<"a">>] = Acc,
  Acc2 = barrel_local:fold_by_id(<<"testdb">>, Fun, [],
                                 [{include_doc, true}, {lt, <<"b">>}]),
  [<<"a">>] = Acc2,
  Acc3 = barrel_local:fold_by_id(<<"testdb">>, Fun, [],
                                 [{include_doc, true}, {lte, <<"b">>}]),
  [<<"b">>, <<"a">>] = Acc3,
  Acc4 = barrel_local:fold_by_id(<<"testdb">>, Fun, [],
                                 [{include_doc, true}, {gte, <<"b">>}]),
  [<<"c">>, <<"b">>] = Acc4,
  Acc5 = barrel_local:fold_by_id(<<"testdb">>, Fun, [],
                                 [{include_doc, true}, {gt, <<"b">>}]),
  [<<"c">>] = Acc5,
  ok.

change_since(_Config) ->
  Fun = fun(Change, Acc) ->
          Id = maps:get(<<"id">>, Change),
          {ok, [Id|Acc]}
        end,
  [] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  [<<"aa">>] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_local:put(<<"testdb">>, Doc2, []),
  {ok, _} = barrel_local:get(<<"testdb">>, <<"bb">>, []),
  [<<"bb">>, <<"aa">>] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  [<<"bb">>] = barrel_local:changes_since(<<"testdb">>, 1, Fun, []),
  [] = barrel_local:changes_since(<<"testdb">>, 2, Fun, []),
  Doc3 = #{ <<"id">> => <<"cc">>, <<"v">> => 1},
  {ok, <<"cc">>, _RevId3} = barrel_local:put(<<"testdb">>, Doc3, []),
  [<<"cc">>] = barrel_local:changes_since(<<"testdb">>, 2, Fun, []),
  ok.

change_deleted(_Config) ->
  Fun = fun(Change, Acc) ->
          Id = maps:get(<<"id">>, Change),
          Del = maps:get(<<"deleted">>, Change, false),
          {ok, [{Id, Del}|Acc]}
        end,
  [] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  [{<<"aa">>, false}] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, RevId2} = barrel_local:put(<<"testdb">>, Doc2, []),
  {ok, _} = barrel_local:get(<<"testdb">>, <<"bb">>, []),
  [{<<"bb">>, false}, {<<"aa">>, false}] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  [{<<"bb">>, false}] = barrel_local:changes_since(<<"testdb">>, 1, Fun, []),
  {ok, <<"bb">>, _} = barrel_local:delete(<<"testdb">>, <<"bb">>, RevId2, []),
  [{<<"bb">>, true}] = barrel_local:changes_since(<<"testdb">>, 2, Fun, []),
  [{<<"bb">>, true}, {<<"aa">>, false}] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  ok.

change_since_include_doc(_Config) ->
  Fun =
    fun(Change, Acc) ->
      {ok, [{maps:get(<<"seq">>, Change), maps:get(<<"doc">>, Change)} |Acc]}
    end,
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  {ok, Doc1} = barrel_local:get(<<"testdb">>, <<"aa">>, []),
  [Change] = barrel_local:changes_since(<<"testdb">>, 0, Fun, [], [{include_doc, true}]),
  {1, Doc1} = Change,
  ok.

change_since_many(_Config) ->
  Fun = fun(Change, Acc) ->
            {ok, [{maps:get(<<"seq">>, Change), Change}|Acc]}
        end,

  %% No changes. Database is empty.
  [] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),

  %% Add 20 docs (doc1 to doc20).
  AddDoc = fun(N) ->
               K = integer_to_binary(N),
               Key = <<"doc", K/binary>>,
               Doc = #{ <<"id">> => Key, <<"v">> => 1},
    {ok, Key, _RevId} = barrel_local:put(<<"testdb">>, Doc, [])
           end,
  [AddDoc(N) || N <- lists:seq(1,20)],

  %% Delete doc1
  {ok, Doc1} = barrel_local:get(<<"testdb">>, <<"doc1">>, []),
  #{<<"_rev">> := RevId} = Doc1,
  {ok, <<"doc1">>, _} = barrel_local:delete(<<"testdb">>, <<"doc1">>, RevId, []),

  %% 20 changes (for doc1 to doc20)
  All = barrel_local:changes_since(<<"testdb">>, 0, Fun, [], [{history, all}]),
  20 = length(All),
  %% History for doc1 includes creation and deletion
  {21, #{<<"changes">> := HistoryDoc1}} = hd(All),
  2 = length(HistoryDoc1),
  
  [{21, #{<<"id">> := <<"doc1">>}},
   {20, #{<<"id">> := <<"doc20">>}},
   {19, #{<<"id">> := <<"doc19">>}}] = barrel_local:changes_since(<<"testdb">>, 18, Fun, []),
  [{21, #{<<"id">> := <<"doc1">>}},
   {20, #{<<"id">> := <<"doc20">>}}] = barrel_local:changes_since(<<"testdb">>, 19, Fun, []),
  [] = barrel_local:changes_since(<<"testdb">>, 21, Fun, []),
  ok.

revsdiff(_Config) ->
  Doc = #{ <<"id">> => <<"revsdiff">>, <<"v">> => 1},
  {ok, <<"revsdiff">>, RevId} = barrel_local:put(<<"testdb">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId, <<"v">> => 2},
  {ok, <<"revsdiff">>, _RevId3} = barrel_local:put(<<"testdb">>, Doc2, []),
  {ok, [<<"1-missing">>], []} = barrel_local:revsdiff(<<"testdb">>, <<"revsdiff">>, [<<"1-missing">>]),
  ok.