%% Copyright 2017, Benoit Chesneau
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

-module(barrel_peer_SUITE).
-author("benoitc").


-define(DB_URL,<<"http://localhost:7080/dbs/testdb">>).
-define(DB_URL2,<<"http://localhost:7080/dbs/source">>).

%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([
  db_ops/1,
  basic_op/1,
  update_doc/1,
  bad_doc/1,
  create_doc/1,
  get_revisions/1,
  fold_by_id/1,
  order_by_key/1,
  multiple_post/1,
  multiple_get/1,
  multiple_delete/1,
  change_since/1,
  change_deleted/1,
  change_since_include_doc/1,
  change_since_many/1
]).

all() ->
  [ db_ops
  , basic_op
  , update_doc
  , bad_doc
  , create_doc
  , get_revisions
  , fold_by_id
  , order_by_key
  , multiple_post
  , multiple_get
  , multiple_delete
  , change_since
  , change_deleted
  , change_since_include_doc
  , change_since_many
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel_store),
  {ok, _} = application:ensure_all_started(barrel_peer),
  Config.

init_per_testcase(_, Config) ->
  ok = barrel_peer:create_database(?DB_URL),
  ok = barrel_peer:create_database(?DB_URL2),
  {ok, Conn} = barrel_peer:connect(?DB_URL),
  [{db, Conn} | Config].

end_per_testcase(_, _Config) ->
  _ = barrel_peer:delete_database(?DB_URL),
  _ = barrel_peer:delete_database(?DB_URL2),
  ok.

end_per_suite(Config) ->
  Config.


db(Config) -> proplists:get_value(db, Config).


db_ops(_Config) ->
  {error, not_found} = barrel_peer:connect(<<"http://localhost:7080/dbs/test-nofound">>),
  [<<"source">>, <<"testdb">>] = barrel_peer:database_names(<<"http://localhost:7080">>),
  {error, not_found} = barrel_peer:connect(<<"http://localhost:7080/dbs/testdb2">>),
  ok = barrel_peer:create_database(<<"http://localhost:7080/dbs/testdb2">>),
  [<<"source">>, <<"testdb">>, <<"testdb2">>] = barrel_peer:database_names(<<"http://localhost:7080">>),
  {ok, _Conn} = barrel_peer:connect(<<"http://localhost:7080/dbs/testdb2">>),
  ok = barrel_peer:delete_database(<<"http://localhost:7080/dbs/testdb2">>),
  {error, not_found} = barrel_peer:connect(<<"http://localhost:7080/dbs/testdb2">>),
  [<<"source">>, <<"testdb">>] = barrel_peer:database_names(<<"http://localhost:7080">>),
  ok.

basic_op(Config) ->
  {error, not_found} = barrel_peer:get(db(Config), <<"a">>, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_peer:post(db(Config), Doc, []),
  {ok, Doc, #{ <<"rev">> := RevId }} = barrel_peer:get(db(Config), <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_peer:delete(db(Config), <<"a">>, [{rev, RevId}]),
  {error, not_found} = barrel_peer:get(db(Config), <<"a">>, []).

update_doc(Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_peer:post(db(Config), Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc, #{<<"rev">> := RevId}} = barrel_peer:get(db(Config), <<"a">>, []),
  Doc3 = Doc2#{ <<"v">> => 2},
  {ok, <<"a">>, RevId2} = barrel_peer:put(db(Config), Doc3, []),
  true = (RevId =/= RevId2),
  {ok, Doc3,_} = barrel_peer:get(db(Config), <<"a">>, []),
  {ok, <<"a">>, _} = barrel_peer:delete(db(Config), <<"a">>, [{rev, RevId2}]),
  {error, not_found} = barrel_peer:get(db(Config), <<"a">>, []),
  {ok, <<"a">>, _} = barrel_peer:put(db(Config), Doc, []).

bad_doc(Config) ->
  Doc = #{ <<"v">> => 1},
  try barrel_peer:put(db(Config), Doc, [])
  catch
    error:{bad_doc, invalid_docid} -> ok
  end.

create_doc(Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, _RevId} = barrel_peer:post(db(Config), Doc, []),
  CreatedDoc = Doc#{ <<"id">> => DocId},
  {ok, CreatedDoc,_ } = barrel_peer:get(db(Config), DocId, []),
  {error, conflict} = barrel_peer:post(db(Config), CreatedDoc, []),
  Doc2 = #{<<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_peer:post(db(Config), Doc2, []).

get_revisions(Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_peer:post(db(Config), Doc, []),
  {ok, Doc2, _} = barrel_peer:get(db(Config), DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_peer:put(db(Config), Doc3, []),
  {ok, Doc4, _} = barrel_peer:get(db(Config), DocId, [{history, true}]),
  Revisions = parse_revisions(Doc4),
  Revisions == [RevId2, RevId].

fold_by_id(Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_peer:post(db(Config), Doc, []),
  Doc2 = #{ <<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_peer:post(db(Config), Doc2, []),
  Doc3 = #{ <<"id">> => <<"c">>, <<"v">> => 1},
  {ok, <<"c">>, _RevId3} = barrel_peer:post(db(Config), Doc3, []),
  Fun = fun
          (#{ <<"id">> := DocId},_Meta, Acc1) ->
            {ok, [DocId | Acc1]}
        end,
  {ok, Acc} = barrel_peer:fold_by_id(db(Config), Fun, [], []),
  [<<"c">>, <<"b">>, <<"a">>] = Acc,
  {ok, Acc2} = barrel_peer:fold_by_id(db(Config), Fun, [], [{lt, <<"b">>}]),
  [<<"a">>] = Acc2,
  {ok, Acc3} = barrel_peer:fold_by_id(db(Config), Fun, [], [{lte, <<"b">>}]),
  [<<"b">>, <<"a">>] = Acc3,
  {ok, Acc4} = barrel_peer:fold_by_id(db(Config), Fun, [], [{gte, <<"b">>}]),
  [<<"c">>, <<"b">>] = Acc4,
  {ok, Acc5} = barrel_peer:fold_by_id(db(Config), Fun, [], [{gt, <<"b">>}]),
  [<<"c">>] = Acc5,
  ok.

order_by_key(Config) ->
  Doc = #{
    <<"id">> => <<"AndersenFamily">>,
    <<"lastName">> => <<"Andersen">>,
    <<"parents">> => [
      #{ <<"firstName">> => <<"Thomas">> },
      #{ <<"firstName">> => <<"Mary Kay">>}
    ],
    <<"children">> => [
      #{
        <<"firstName">> => <<"Henriette Thaulow">>, <<"gender">> => <<"female">>, <<"grade">> =>  5,
        <<"pets">> => [#{ <<"givenName">> => <<"Fluffy">> }]
      }
    ],
    <<"address">> => #{ <<"state">> => <<"WA">>, <<"county">> => <<"King">>, <<"city">> => <<"seattle">> },
    <<"creationDate">> => 1431620472,
    <<"isRegistered">> => true
  },
  {ok, <<"AndersenFamily">>, _Rev} = barrel_peer:post(db(Config), Doc, []),
  timer:sleep(400),
  {ok, Doc1, _} = barrel_peer:get(db(Config), <<"AndersenFamily">>, []),
  Fun = fun(Obj, Acc) -> {ok, [Obj| Acc]} end,
                                           {ok,
    [#{<<"id">> := <<"AndersenFamily">>,
      <<"val">> := <<"AndersenFamily">>}]} = barrel_peer:fold_by_path(db(Config), <<"id">>, Fun, [], []),
  {ok, [Obj]} = barrel_peer:fold_by_path(db(Config), <<"id">>, Fun, [], [{include_docs, true}] ),
  #{<<"doc">> := Doc1} = Obj,
  ok.

multiple_post(Config) ->
  Self = self(),
  Pids  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Doc = #{ <<"id">> => DocId, <<"val">> => I},
      Pid = spawn_link(
        fun() ->
          {ok, DocId, _} = barrel_peer:post(db(Config), Doc, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids),
  #{ <<"docs_count">> := 500 } = barrel_peer:database_infos(?DB_URL),
  ok.


multiple_get(Config) ->
  Self = self(),
  Pids  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Doc = #{ <<"id">> => DocId, <<"val">> => I},
      Pid = spawn_link(
        fun() ->
          {ok, DocId, _} = barrel_peer:post(db(Config), Doc, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids),
  #{ <<"docs_count">> := 500 } = barrel_peer:database_infos(?DB_URL),
  Pids1  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Pid = spawn_link(
        fun() ->
          {ok, #{ <<"id">> := DocId }, _} = barrel_peer:get(db(Config), DocId, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids1),
  ok.

multiple_delete(Config) ->
  Self = self(),
  Pids  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Doc = #{ <<"id">> => DocId, <<"val">> => I},
      Pid = spawn_link(
        fun() ->
          {ok, DocId, _} = barrel_peer:post(db(Config), Doc, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids),
  #{ <<"docs_count">> := 500 } = barrel_peer:database_infos(?DB_URL),
  Pids1  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Pid = spawn_link(
        fun() ->
          {ok, #{ <<"id">> := DocId}, #{ <<"rev">> := Rev }} = barrel_peer:get(db(Config), DocId, []),
          {ok, _, _} = barrel_peer:delete(db(Config), DocId, [{rev, Rev}]),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids1),
  #{ <<"docs_count">> := 0 } = barrel_peer:database_infos(?DB_URL),
  ok.


change_since(Config) ->
  Fun = fun(Change, Acc) ->
    Id = maps:get(<<"id">>, Change),
    {ok, [Id|Acc]}
        end,
  {ok, []} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_peer:post(db(Config), Doc, []),
  {ok, [<<"aa">>]} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_peer:post(db(Config), Doc2, []),
  {ok, _, _} = barrel_peer:get(db(Config), <<"bb">>, []),
  {ok, [<<"bb">>, <<"aa">>]} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),
  {ok, [<<"bb">>]} = barrel_peer:changes_since(db(Config), 1, Fun, [], []),
  {ok, []} = barrel_peer:changes_since(db(Config), 2, Fun, [], []),
  Doc3 = #{ <<"id">> => <<"cc">>, <<"v">> => 1},
  {ok, <<"cc">>, _RevId3} = barrel_peer:post(db(Config), Doc3, []),
  {ok, [<<"cc">>]} = barrel_peer:changes_since(db(Config), 2, Fun, [], []),
  ok.

change_deleted(Config) ->
  Fun = fun(Change, Acc) ->
    Id = maps:get(<<"id">>, Change),
    Del = maps:get(<<"deleted">>, Change, false),
    {ok, [{Id, Del}|Acc]}
        end,
  {ok, []} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_peer:post(db(Config), Doc, []),
  {ok, [{<<"aa">>, false}]} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, RevId2} = barrel_peer:post(db(Config), Doc2, []),
  {ok, _, _} = barrel_peer:get(db(Config), <<"bb">>, []),
  {ok, [{<<"bb">>, false}, {<<"aa">>, false}]} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),
  {ok, [{<<"bb">>, false}]} = barrel_peer:changes_since(db(Config), 1, Fun, [], []),
  {ok, <<"bb">>, _} = barrel_peer:delete(db(Config), <<"bb">>, [{rev, RevId2}]),
  {ok, [{<<"bb">>, true}]} = barrel_peer:changes_since(db(Config), 2, Fun, [], []),
  {ok, [{<<"bb">>, true}, {<<"aa">>, false}]} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),
  ok.



change_since_include_doc(Config) ->
  Fun =
  fun(Change, Acc) ->
    Seq = maps:get(<<"seq">>, Change),
    {ok, [{Seq, maps:get(<<"doc">>, Change)} |Acc]}
  end,
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_peer:post(db(Config), Doc, []),
  {ok, Doc1, _Meta} = barrel_peer:get(db(Config), <<"aa">>, []),
  {ok, [Change]} = barrel_peer:changes_since(db(Config), 0, Fun, [], [{<<"include_doc">>, <<"true">>}]),
  {1, Doc1} = Change,
  ok.

change_since_many(Config) ->
  Fun = fun(Change, Acc) ->
          Seq = maps:get(<<"seq">>, Change),
          {ok, [{Seq, Change}|Acc]}
        end,

  %% No changes. Database is empty.
  {ok, []} = barrel_peer:changes_since(db(Config), 0, Fun, [], []),

  %% Add 20 docs (doc1 to doc20).
  AddDoc = fun(N) ->
              K = integer_to_binary(N),
              Key = <<"doc", K/binary>>,
              Doc = #{ <<"id">> => Key, <<"v">> => 1},
    {ok, Key, _RevId} = barrel_peer:post(db(Config), Doc, [])
           end,
  [AddDoc(N) || N <- lists:seq(1,20)],

  %% Delete doc1
  {ok, _Doc1, #{<<"rev">> := RevId}} = barrel_peer:get(db(Config), <<"doc1">>, []),
  {ok, <<"doc1">>, _} = barrel_peer:delete(db(Config), <<"doc1">>, [{rev, RevId}]),

  %% 20 changes (for doc1 to doc20)
  {ok, All} = barrel_peer:changes_since(db(Config), 0, Fun, [], [{history, all}]),
  20 = length(All),
  %% History for doc1 includes creation and deletion
  {21, #{<<"changes">> := HistoryDoc1}} = hd(All),
  2 = length(HistoryDoc1),

  {ok, [{21, #{<<"id">> := <<"doc1">>}},
        {20, #{<<"id">> := <<"doc20">>}},
        {19, #{<<"id">> := <<"doc19">>}}]} = barrel_peer:changes_since(db(Config), 18, Fun, [], []),
  {ok, [{21, #{<<"id">> := <<"doc1">>}},
        {20, #{<<"id">> := <<"doc20">>}}]} = barrel_peer:changes_since(db(Config), 19, Fun, [], []),
  {ok, []} = barrel_peer:changes_since(db(Config), 21, Fun, [], []),
  ok.

%% internal

wait_pids([]) -> ok;
wait_pids(Pids) ->
  receive
    {ok, Pid} -> wait_pids(Pids -- [Pid])
  after 5000 -> {error, receive_pids}
  end.


%% from barrel_doc in barrel_commons


parse_revisions(#{ <<"_revisions">> := Revisions}) ->
  case Revisions of
    #{ <<"start">> := Start, <<"ids">> := Ids} ->
      {Revs, _} = lists:foldl(
        fun(Id, {Acc, I}) ->
          Acc2 = [<< (integer_to_binary(I))/binary,"-", Id/binary >> | Acc],
          {Acc2, I - 1}
        end, {[], Start}, Ids),
      lists:reverse(Revs);
    _ -> []
  end;
parse_revisions(#{<<"_rev">> := Rev}) -> [Rev];
parse_revisions(_) -> [].

