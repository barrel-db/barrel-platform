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
  missing_db/1,
  open_db/1,
  database_names/1,
  basic_op/1,
  update_doc/1,
  update_doc_lwww/1,
  create_doc/1,
  fold_by_id/1,
  change_since/1,
  change_since_many/1,
  change_since_include_doc/1,
  revdiff/1,
  get_revisions/1,
  put_rev/1
]).

all() ->
  [
    store_exists,
    missing_db,
    open_db,
    database_names,
    basic_op,
    update_doc,
    update_doc_lwww,
    create_doc,
    get_revisions,
    fold_by_id,
    change_since,
    change_since_many,
    change_since_include_doc,
    revdiff,
    put_rev
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
  Config.


init_per_testcase(_, Config) ->
  {true, Conn} = barrel:create_database(barrel_test_rocksdb, <<"testdb">>),
  [{conn, Conn} | Config].

end_per_testcase(_, Config) ->
  Conn = proplists:get_value(conn, Config),
  barrel:delete_database(Conn),
  ok.

end_per_suite(Config) ->
  erocksdb:destroy("data/testdb", []),
  Config.

store_exists(_Config) ->
  true = filelib:is_dir(<<"data/testdb">>),
  ok.

missing_db(_Config) ->
  {error, not_found} = barrel:connect_database(barrel_test_rocksdb, <<"testdb_unknown">>),
  ok.


open_db(Config) ->
  Conn = proplists:get_value(conn, Config),
  {ok, Infos} = barrel:database_infos(Conn),
  Id = maps:get(id, Infos),
  {ok, Infos2} = barrel:database_infos(Conn),
  Id = maps:get(id, Infos2),
  ok = barrel:delete_database(Conn),
  {error, not_found} = barrel:connect_database(barrel_test_rocksdb, <<"testdb">>),
  {true, _} = barrel:create_database(barrel_test_rocksdb, <<"testdb">>),
  %% can start it several times
  {false, _} = barrel:create_database(barrel_test_rocksdb, <<"testdb">>),
  {ok, Conn2} = barrel:connect_database(barrel_test_rocksdb, <<"testdb">>),
  {ok, Infos3} = barrel:database_infos(Conn2),
  true = (Id /= maps:get(id, Infos3)).

database_names(Config) ->
  Conn = proplists:get_value(conn, Config),
  [<<"testdb">>] = barrel:database_names(barrel_test_rocksdb),
  {true, Conn1} = barrel:create_database(barrel_test_rocksdb, <<"testdb1">>),
  [<<"testdb">>, <<"testdb1">>] = barrel:database_names(barrel_test_rocksdb),
  {true, Conn2} = barrel:create_database(barrel_test_rocksdb, <<"testdb2">>),
  [<<"testdb">>, <<"testdb1">>, <<"testdb2">>] = barrel:database_names(barrel_test_rocksdb),
  ok = barrel:close_database(Conn1),
  [<<"testdb">>, <<"testdb1">>, <<"testdb2">>] = barrel:database_names(barrel_test_rocksdb),
  ok =  barrel:delete_database(Conn1),
  [<<"testdb">>, <<"testdb2">>] = barrel:database_names(barrel_test_rocksdb),
  ok =  barrel:delete_database(Conn2),
  [<<"testdb">>] = barrel:database_names(barrel_test_rocksdb),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(Conn, <<"a">>, Doc, []),
  [<<"testdb">>] = barrel:database_names(barrel_test_rocksdb).



basic_op(Config) ->
  Conn = proplists:get_value(conn, Config),
  {error, not_found} = barrel_db:get(Conn, <<"a">>, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(Conn, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_db:get(Conn, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_db:delete(Conn, <<"a">>, RevId, []),
  {ok, DeletedDoc} = barrel_db:get(Conn, <<"a">>, []),
  true = maps:get(<<"_deleted">>, DeletedDoc).

update_doc(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_db:put(Conn, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_db:get(Conn, <<"a">>, []),
  Doc3 = Doc2#{ v => 2},
  {ok, <<"a">>, RevId2} = barrel_db:put(Conn, <<"a">>, Doc3, []),
  true = (RevId =/= RevId2),
  Doc4 = Doc3#{<<"_rev">> => RevId2},
  {ok, Doc4} = barrel_db:get(Conn, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_db:delete(Conn, <<"a">>, RevId2, []),
  {ok, DeletedDoc} = barrel_db:get(Conn, <<"a">>, []),
  true = maps:get(<<"_deleted">>, DeletedDoc),
  {ok, <<"a">>, _RevId3} = barrel_db:put(Conn, <<"a">>, Doc, []).

update_doc_lwww(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(Conn, <<"a">>, Doc, []),
  {ok, Doc2} = barrel_db:get(Conn, <<"a">>, []),
  #{ <<"v">> := 1 } = Doc2,

  Doc3 = #{ <<"id">> => <<"a">>, <<"v">> => 2},
  {error, {conflict, doc_exists}} = barrel_db:put(Conn, <<"a">>, Doc3, []),

  {ok, <<"a">>, _RevId2} = barrel_db:put(Conn, <<"a">>, Doc3, [{lww, true}]),
  {ok, Doc4} = barrel_db:get(Conn, <<"a">>, []),
  #{ <<"v">> := 2 } = Doc4.

create_doc(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} =  barrel_db:post(Conn, Doc, []),
  CreatedDoc = Doc#{ <<"id">> => DocId, <<"_rev">> => RevId},
  {ok, CreatedDoc} = barrel_db:get(Conn, DocId, []),
  {error, not_found} =  barrel_db:post(Conn, CreatedDoc, []),
  Doc2 = #{<<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} =  barrel_db:post(Conn, Doc2, []).

get_revisions(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} =  barrel_db:post(Conn, Doc, []),
  {ok, Doc2} = barrel_db:get(Conn, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_db:put(Conn, DocId, Doc3, []),
  {ok, Doc4} = barrel_db:get(Conn, DocId, [{history, true}]),
  Revisions = barrel_doc:parse_revisions(Doc4),
  Revisions == [RevId2, RevId].

put_rev(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} =  barrel_db:post(Conn, Doc, []),
  {ok, Doc2} = barrel_db:get(Conn, DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_db:put(Conn, DocId, Doc3, []),

  Doc4_0 = Doc2#{ v => 3 },
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc4_0),
  Doc4 = Doc4_0#{<<"_rev">> => NewRev},
  History = [NewRev, RevId],
 
  {ok, DocId, _RevId3} = barrel_db:put_rev(Conn, DocId, Doc4, History, []),
  {ok, Doc5} = barrel_db:get(Conn, DocId, [{history, true}]),
  Revisions = barrel_doc:parse_revisions(Doc5),
  Revisions == [RevId2, RevId].


fold_by_id(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_db:put(Conn, <<"a">>, Doc, []),
  Doc2 = #{ <<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_db:put(Conn, <<"b">>, Doc2, []),
  Fun = fun(DocId, _DocInfo, {ok, FoldDoc}, Acc1) ->
      DocId = barrel_doc:id(FoldDoc),
      {ok, [DocId | Acc1]}
    end,
  Acc = barrel_db:fold_by_id(Conn, Fun, [], [{include_doc, true}]),
  [<<"b">>, <<"a">>] = Acc,
  Acc2 = barrel_db:fold_by_id(Conn, Fun, [],
                              [{include_doc, true}, {start_key, <<"b">>}]),
  [<<"b">>] = Acc2.

change_since(Config) ->
  Conn = proplists:get_value(conn, Config),
  Fun = fun(_Seq, DocInfo, _Doc, Acc) ->
                  Id = maps:get(id, DocInfo),
                  {ok, [Id|Acc]}
        end,
  [] = barrel:changes_since(Conn, 0, Fun, []),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_db:put(Conn, <<"aa">>, Doc, []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_db:put(Conn, <<"bb">>, Doc2, []),
  [<<"bb">>, <<"aa">>] = barrel:changes_since(Conn, 0, Fun, []),
  [<<"bb">>] = barrel:changes_since(Conn, 1, Fun, []),
  [] = barrel:changes_since(Conn, 2, Fun, []),
  {ok, <<"cc">>, _RevId2} = barrel_db:put(Conn, <<"cc">>, Doc2, []),
  [<<"cc">>] = barrel:changes_since(Conn, 2, Fun, []),
  ok.

change_since_include_doc(Config) ->
  Conn = proplists:get_value(conn, Config),
  Fun =
    fun(Seq, _DocInfo, Doc, Acc) ->
      {ok, [{Seq, Doc} |Acc]}
    end,
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_db:put(Conn, <<"aa">>, Doc, []),
  {ok, Doc1} = barrel_db:get(Conn, <<"aa">>, []),
  [Change] = barrel:changes_since(Conn, 0, Fun, [], [{include_doc, true}]),
  {1, {ok, Doc1}} = Change,
  ok.

change_since_many(Config) ->
  Conn = proplists:get_value(conn, Config),
  Fun = fun(_Seq, DocInfo, _Doc, Acc) ->
            Id = maps:get(id, DocInfo),
            {ok, [Id|Acc]}
        end,
  [] = barrel:changes_since(Conn, 0, Fun, []),
  AddDoc = fun(N) ->
               K = integer_to_binary(N),
               Key = <<"doc", K/binary>>,
               Doc = #{ <<"id">> => Key, <<"v">> => 1},
               {ok, Key, _RevId} = barrel_db:put(Conn, Key, Doc, [])
           end,
  [AddDoc(N) || N <- lists:seq(1,20)],

  20 = length(barrel:changes_since(Conn, 0, Fun, [])),
  [<<"doc20">>, <<"doc19">>] = barrel:changes_since(Conn, 18, Fun, []),
  [<<"doc20">>] = barrel:changes_since(Conn, 19, Fun, []),
  [] = barrel:changes_since(Conn, 20, Fun, []),
  ok.

revdiff(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{ <<"id">> => <<"revdiff">>, <<"v">> => 1},
  {ok, <<"revdiff">>, RevId} = barrel_db:put(Conn, <<"revdiff">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId, <<"v">> => 2},
  {ok, <<"revdiff">>, _RevId3} = barrel_db:put(Conn, <<"revdiff">>, Doc2, []),
  {ok, [<<"1-missing">>], []} = barrel_db:revsdiff(Conn, <<"revdiff">>, [<<"1-missing">>]),
  ok.
