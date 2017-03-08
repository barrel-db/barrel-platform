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
  multi_get/1,
  put_is_not_create/1,
  deletet_is_not_create/1,
  last_write_win/1,
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
    multi_get,
    put_is_not_create,
    deletet_is_not_create,
    last_write_win,
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
  timer:sleep(10),
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
  {ok, <<"a">>, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc, #{<<"rev">> := RevId}=Meta} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  false = maps:is_key(<<"deleted">>, Meta),
  {ok, <<"a">>, _RevId2} = barrel_local:delete(<<"testdb">>, <<"a">>, [{rev, RevId}]),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"a">>, []).

update_doc(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc, _Meta2} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  Doc2 = Doc#{ v => 2},
  {ok, <<"a">>, RevId2} = barrel_local:put(<<"testdb">>, Doc2, []),
  true = (RevId =/= RevId2),
  {ok, Doc2, _Meta4} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_local:delete(<<"testdb">>, <<"a">>, [{rev, RevId2}]),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  {ok, <<"a">>, _RevId3} = barrel_local:post(<<"testdb">>, Doc, []).


multi_get(_Config) ->
  %% create some docs
  Kvs = [{<<"a">>, 1},
         {<<"b">>, 2},
         {<<"c">>, 3}],
  Docs = [#{ <<"id">> => K, <<"v">> => V} || {K,V} <- Kvs],
  [ {ok,_,_} = barrel_local:post(<<"testdb">>, D, []) || D <- Docs ],

  %% the "query" to get the id/rev
  Mget = [ Id || {Id, _} <- Kvs],

  %% a fun to parse the results
  %% the parameter is the same format as the regular get function output
  Fun=fun(Doc, Meta, Acc) ->
          #{<<"id">> := DocId} = Doc,
          #{<<"rev">> := RevId} = Meta,
          [#{<<"id">> => DocId, <<"rev">> => RevId, <<"doc">>  => Doc }|Acc]
      end,

  %% let's process it
  Results = barrel_local:multi_get(<<"testdb">>, Fun, [], Mget, []),

  %% check results
  [#{<<"doc">> := #{<<"id">> := <<"a">>, <<"v">> := 1},
     <<"id">> := <<"a">>,
     <<"rev">> := _},
   #{<<"doc">> := #{<<"id">> := <<"b">>, <<"v">> := 2}},
   #{<<"doc">> := #{<<"id">> := <<"c">>, <<"v">> := 3}}] = lists:reverse(Results).


put_is_not_create(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {error, not_found} = barrel_local:put(<<"testdb">>, Doc, []).

deletet_is_not_create(_Config) ->
  {error, not_found} = barrel_local:delete(<<"testdb">>, <<"a">>, []).

last_write_win(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  Doc2 = Doc#{ v => 2},
  {ok, <<"a">>, _RevId2} = barrel_local:put(<<"testdb">>, Doc2, [{rev, RevId}]),
  Doc3 = Doc#{ v => 3},
  {error, {conflict, revision_conflict}} = barrel_local:put(<<"testdb">>, Doc3, [{rev, RevId}]),
  {ok, <<"a">>, << "3-", _/binary >>} = barrel_local:put(<<"testdb">>, Doc3, []).

revision_conflict(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, _, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc1, _} = barrel_local:get(<<"testdb">>, <<"a">>, []),
  Doc2 = Doc1#{ <<"v">> => 2 },
  {ok, <<"a">>, _RevId2} = barrel_local:put(<<"testdb">>, Doc2, [{rev, RevId}]),
  {error, {conflict, revision_conflict}} = barrel_local:put(<<"testdb">>, Doc2, [{rev, RevId}]),
  ok.

async_update(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  ok= barrel_local:post(<<"testdb">>, Doc, [{async, true}]),
  timer:sleep(100),
  {ok, #{ <<"id">> := <<"a">>, <<"v">> := 1}, _} = barrel_local:get(<<"testdb">>, <<"a">>, []).

resource_id(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, _, #{ <<"rev">> := RevId, <<"rid">> := Rid}} = barrel_local:get(<<"testdb">>, <<"a">>, [{meta, true}]),
  1 = barrel_db:decode_rid(Rid),
  {ok, <<"a">>, RevId2} = barrel_local:put(<<"testdb">>, Doc, [{rev, RevId}]),
  {ok, _, #{ <<"rev">> := RevId2, <<"rid">> := Rid}} = barrel_local:get(<<"testdb">>, <<"a">>, [{meta, true}]),
  Doc2 = #{ <<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _} = barrel_local:post(<<"testdb">>, Doc2, []),
  {ok, _, #{ <<"rid">> := Rid2}} = barrel_local:get(<<"testdb">>, <<"b">>, [{meta, true}]),
  2 = barrel_db:decode_rid(Rid2).

bad_doc(_Config) ->
  Doc = #{ <<"v">> => 1},
  try barrel_local:put(<<"testdb">>, Doc, [])
  catch
    error:{bad_doc, invalid_docid} -> ok
  end.

create_doc(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, _RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, CreatedDoc, _} = barrel_local:get(<<"testdb">>, DocId, []),
  {error, {conflict, doc_exists}} = barrel_local:post(<<"testdb">>, CreatedDoc, []),
  {ok, _, _} = barrel_local:post(<<"testdb">>, CreatedDoc, [{is_upsert, true}]),
  Doc2 = #{<<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_local:post(<<"testdb">>, Doc2, []).

docs_count(_Config) ->
  #{ docs_count := 0 } = barrel_local:db_infos(<<"testdb">>),
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  #{ docs_count := 1 } = barrel_local:db_infos(<<"testdb">>),
  {ok, _, _} = barrel_local:delete(<<"testdb">>, DocId, [{rev, RevId}]),
  #{ docs_count := 0 } = barrel_local:db_infos(<<"testdb">>).

get_revisions(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc2, _} = barrel_local:get(<<"testdb">>, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_local:put(<<"testdb">>, Doc3, [{rev, RevId}]),
  {ok, Doc3, Meta} = barrel_local:get(<<"testdb">>, DocId, [{history, true}]),
  Revisions = [RevId2, RevId],
  Revisions = barrel_doc:parse_revisions(Meta).

put_rev(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc2, _} = barrel_local:get(<<"testdb">>, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_local:put(<<"testdb">>, Doc3, [{rev, RevId}]),
  Doc4 = Doc2#{ v => 3 },
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, barrel_doc:make_doc(Doc4, RevId, false)),
  History = [NewRev, RevId],
  Deleted = false,
  {ok, DocId, _RevId3} = barrel_local:put_rev(<<"testdb">>, Doc4, History, Deleted, []),
  {ok, _Doc5, Meta} = barrel_local:get(<<"testdb">>, DocId, [{history, true}]),
  Revisions = [RevId2, RevId],
  io:format("revisions: ~p~nparsed:~p~n", [Revisions, barrel_doc:parse_revisions(Meta)]),
  
  Revisions = barrel_doc:parse_revisions(Meta).



fold_by_id(_Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  Doc2 = #{ <<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_local:post(<<"testdb">>, Doc2, []),
  Doc3 = #{ <<"id">> => <<"c">>, <<"v">> => 1},
  {ok, <<"c">>, _RevId3} = barrel_local:post(<<"testdb">>, Doc3, []),
  Fun = fun(#{ <<"id">> := DocId }, _Meta, Acc1) ->
      {ok, [DocId | Acc1]}
    end,
  Acc = barrel_local:fold_by_id(<<"testdb">>, Fun, [], []),
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
  {ok, <<"aa">>, _RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  [<<"aa">>] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_local:post(<<"testdb">>, Doc2, []),
  {ok, _, _} = barrel_local:get(<<"testdb">>, <<"bb">>, []),
  [<<"bb">>, <<"aa">>] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  [<<"bb">>] = barrel_local:changes_since(<<"testdb">>, 1, Fun, []),
  [] = barrel_local:changes_since(<<"testdb">>, 2, Fun, []),
  Doc3 = #{ <<"id">> => <<"cc">>, <<"v">> => 1},
  {ok, <<"cc">>, _RevId3} = barrel_local:post(<<"testdb">>, Doc3, []),
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
  {ok, <<"aa">>, _RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  [{<<"aa">>, false}] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, RevId2} = barrel_local:post(<<"testdb">>, Doc2, []),
  {ok, _, _} = barrel_local:get(<<"testdb">>, <<"bb">>, []),
  [{<<"bb">>, false}, {<<"aa">>, false}] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  [{<<"bb">>, false}] = barrel_local:changes_since(<<"testdb">>, 1, Fun, []),
  {ok, <<"bb">>, _} = barrel_local:delete(<<"testdb">>, <<"bb">>, [{rev, RevId2}]),
  [{<<"bb">>, true}] = barrel_local:changes_since(<<"testdb">>, 2, Fun, []),
  [{<<"bb">>, true}, {<<"aa">>, false}] = barrel_local:changes_since(<<"testdb">>, 0, Fun, []),
  ok.

change_since_include_doc(_Config) ->
  Fun =
    fun(Change, Acc) ->
      {ok, [{maps:get(<<"seq">>, Change), maps:get(<<"doc">>, Change)} |Acc]}
    end,
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  {ok, Doc1, _} = barrel_local:get(<<"testdb">>, <<"aa">>, []),
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
    {ok, Key, _RevId} = barrel_local:post(<<"testdb">>, Doc, [])
           end,
  [AddDoc(N) || N <- lists:seq(1,20)],

  %% Delete doc1
  {ok, _Doc1, #{<<"rev">> := RevId}} = barrel_local:get(<<"testdb">>, <<"doc1">>, []),
  {ok, <<"doc1">>, _} = barrel_local:delete(<<"testdb">>, <<"doc1">>, [{rev, RevId}]),

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
  {ok, <<"revsdiff">>, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  Doc2 = Doc#{<<"v">> => 2},
  {ok, <<"revsdiff">>, _RevId3} = barrel_local:put(<<"testdb">>, Doc2, [{rev, RevId}]),
  {ok, [<<"1-missing">>], []} = barrel_local:revsdiff(<<"testdb">>, <<"revsdiff">>, [<<"1-missing">>]),
  ok.
