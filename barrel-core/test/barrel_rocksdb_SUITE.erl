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

-module(barrel_rocksdb_SUITE).
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
  store_exists/1,
  open_db/1,
  all_databases/1,
  basic_op/1,
  update_doc/1,
  create_doc/1,
  fold_by_id/1,
  change_since/1,
  revdiff/1,
  get_revisions/1,
  put_rev/1
]).

all() ->
  [
    store_exists,
    open_db,
    all_databases,
    basic_op,
    update_doc,
    create_doc,
    get_revisions,
    fold_by_id,
    change_since,
    revdiff,
    put_rev
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
  Config.


init_per_testcase(_, Config) ->
  ok = barrel_db:start(<<"testdb">>, barrel_test_rocksdb),
  Config.

end_per_testcase(_, _Config) ->
  barrel_db:clean(<<"testdb">>),
  ok.

end_per_suite(Config) ->
  erocksdb:destroy("testdb", []),
  Config.

store_exists(_Config) ->
  true = filelib:is_dir(<<"testdb">>),
  ok.

open_db(_Config) ->
  {ok, Infos} = barrel_db:infos(<<"testdb">>),
  Id = maps:get(id, Infos),
  {ok, Infos2} = barrel_db:infos(<<"testdb">>),
  Id = maps:get(id, Infos2),
  ok = barrel_db:clean(<<"testdb">>),
  ok = barrel_db:start(<<"testdb">>, barrel_test_rocksdb),
  {ok, Infos3} = barrel_db:infos(<<"testdb">>),
  true = (Id /= maps:get(id, Infos3)).

all_databases(_Config) ->
  [<<"testdb">>] = barrel:all_databases(),
  ok = barrel_db:start(<<"testdb1">>, barrel_test_rocksdb),
  [<<"testdb">>, <<"testdb1">>] = barrel:all_databases(),
  ok = barrel_db:start(<<"testdb2">>, barrel_test_rocksdb),
  [<<"testdb">>, <<"testdb1">>, <<"testdb2">>] = barrel:all_databases(),
  ok = barrel_db:stop(<<"testdb1">>),
  [<<"testdb">>, <<"testdb1">>, <<"testdb2">>] = barrel:all_databases(),
  ok = barrel_db:start(<<"testdb1">>, barrel_test_rocksdb),
  ok =  barrel_db:clean(<<"testdb1">>),
  [<<"testdb">>, <<"testdb2">>] = barrel:all_databases(),
  ok =  barrel_db:clean(<<"testdb2">>),
  [<<"testdb">>] = barrel:all_databases(),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(<<"testdb">>, <<"a">>, Doc, []),
  [<<"testdb">>] = barrel:all_databases().
  


basic_op(_Config) ->
  {error, not_found} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(<<"testdb">>, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_db:delete(<<"testdb">>, <<"a">>, RevId, []),
  {ok, DeletedDoc} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  true = maps:get(<<"_deleted">>, DeletedDoc).

update_doc(_Config) ->
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(<<"testdb">>, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  Doc3 = Doc2#{ v => 2},
  {ok, <<"a">>, RevId2} = barrel_db:put(<<"testdb">>, <<"a">>, Doc3, []),
  true = (RevId =/= RevId2),
  Doc4 = Doc3#{<<"_rev">> => RevId2},
  {ok, Doc4} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_db:delete(<<"testdb">>, <<"a">>, RevId2, []),
  {ok, DeletedDoc} = barrel_db:get(<<"testdb">>, <<"a">>, []),
  true = maps:get(<<"_deleted">>, DeletedDoc),
  {ok, <<"a">>, _RevId3} = barrel_db:put(<<"testdb">>, <<"a">>, Doc, []).

create_doc(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} =  barrel_db:post(<<"testdb">>, Doc, []),
  CreatedDoc = Doc#{ <<"_id">> => DocId, <<"_rev">> => RevId},
  {ok, CreatedDoc} = barrel_db:get(<<"testdb">>, DocId, []),
  {error, not_found} =  barrel_db:post(<<"testdb">>, CreatedDoc, []),
  Doc2 = #{<<"_id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} =  barrel_db:post(<<"testdb">>, Doc2, []).

get_revisions(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} =  barrel_db:post(<<"testdb">>, Doc, []),
  {ok, Doc2} = barrel_db:get(<<"testdb">>, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_db:put(<<"testdb">>, DocId, Doc3, []),
  {ok, Doc4} = barrel_db:get(<<"testdb">>, DocId, [{history, true}]),
  Revisions = barrel_doc:parse_revisions(Doc4),
  Revisions == [RevId2, RevId].

put_rev(_Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} =  barrel_db:post(<<"testdb">>, Doc, []),
  {ok, Doc2} = barrel_db:get(<<"testdb">>, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_db:put(<<"testdb">>, DocId, Doc3, []),

  Doc4_0 = Doc2#{ v => 3 },
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc4_0),
  Doc4 = Doc4_0#{<<"_rev">> => NewRev},
  History = [NewRev, RevId],
 
  {ok, DocId, RevId3} = barrel_db:put_rev(<<"testdb">>, DocId, Doc4, History, []),
  {ok, Doc5} = barrel_db:get(<<"testdb">>, DocId, [{history, true}]),
  Revisions = barrel_doc:parse_revisions(Doc5),
  Revisions == [RevId2, RevId].


fold_by_id(_Config) ->
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(<<"testdb">>, <<"a">>, Doc, []),
  Doc2 = #{ <<"_id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_db:put(<<"testdb">>, <<"b">>, Doc2, []),
  Fun = fun(DocId, _DocInfo, {ok, FoldDoc}, Acc1) ->
      DocId = barrel_doc:id(FoldDoc),
      {ok, [DocId | Acc1]}
    end,
  Acc = barrel_db:fold_by_id(<<"testdb">>, Fun, [], [{include_doc, true}]),
  [<<"b">>, <<"a">>] = Acc,
  Acc2 = barrel_db:fold_by_id(<<"testdb">>, Fun, [],
                              [{include_doc, true}, {start_key, <<"b">>}]),
  [<<"b">>] = Acc2.

change_since(_Config) ->
  Fun = fun(_Seq, DocInfo, _Doc, Acc) ->
                  Id = maps:get(id, DocInfo),
                  {ok, [Id|Acc]}
        end,
  [] = barrel_db:changes_since(<<"testdb">>, 0, Fun, []),
  Doc = #{ <<"_id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_db:put(<<"testdb">>, <<"aa">>, Doc, []),
  Doc2 = #{ <<"_id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_db:put(<<"testdb">>, <<"bb">>, Doc2, []),
  [<<"bb">>, <<"aa">>] = barrel_db:changes_since(<<"testdb">>, 0, Fun, []),
  [<<"bb">>] = barrel_db:changes_since(<<"testdb">>, 1, Fun, []),
  [] = barrel_db:changes_since(<<"testdb">>, 2, Fun, []),
  {ok, <<"cc">>, _RevId2} = barrel_db:put(<<"testdb">>, <<"cc">>, Doc2, []),
  [<<"cc">>] = barrel_db:changes_since(<<"testdb">>, 2, Fun, []),
  ok.

revdiff(_Config) ->
  Doc = #{ <<"_id">> => <<"revdiff">>, <<"v">> => 1},
  {ok, <<"revdiff">>, RevId} = barrel_db:put(<<"testdb">>, <<"revdiff">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId, <<"v">> => 2},
  {ok, <<"revdiff">>, _RevId3} = barrel_db:put(<<"testdb">>, <<"revdiff">>, Doc2, []),
  {ok, [<<"1-missing">>], []} = barrel_db: revsdiff(<<"testdb">>, <<"revdiff">>, [<<"1-missing">>]),
  ok.
