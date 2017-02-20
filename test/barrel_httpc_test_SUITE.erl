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

-module(barrel_httpc_test_SUITE).
-author("benoitc").


-define(DB_URL,<<"http://localhost:7080/dbs/testdb">>).

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
  put_rev/1,
  fold_by_id/1,
  order_by_key/1,
  multiple_put/1,
  multiple_get/1,
  multiple_delete/1,
  change_since/1,
  change_deleted/1,
  change_since_include_doc/1,
  change_since_many/1,
  revsdiff/1,
  system_docs/1
]).

all() ->
  [
    db_ops,
    basic_op,
    update_doc,
    bad_doc,
    create_doc,
    get_revisions,
    put_rev,
    fold_by_id,
    order_by_key,
    multiple_put,
    multiple_get,
    multiple_delete,
    change_since,
    change_deleted,
    change_since_include_doc,
    change_since_many,
    revsdiff
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel_store),
  {ok, _} = application:ensure_all_started(barrel_httpc),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  _ = barrel_store:create_db(<<"source">>, #{}),
  {ok, Conn} = barrel_httpc:connect(?DB_URL),
  [{db, Conn} | Config].

end_per_testcase(_, _Config) ->
  ok = barrel_local:delete_db(<<"testdb">>),
  ok = barrel_local:delete_db(<<"source">>),
  ok.

end_per_suite(Config) ->
  Config.


db(Config) -> proplists:get_value(db, Config).


db_ops(_Config) ->
  {error, not_found} = barrel_httpc:connect(<<"http://localhost:7080/dbs/test-nofound">>),
  [<<"source">>, <<"testdb">>] = barrel_httpc:database_names(<<"http://localhost:7080">>),
  {error, not_found} = barrel_httpc:connect(<<"http://localhost:7080/dbs/testdb2">>),
  ok = barrel_httpc:create_database(<<"http://localhost:7080/dbs/testdb2">>),
  [<<"source">>, <<"testdb">>, <<"testdb2">>] = barrel_httpc:database_names(<<"http://localhost:7080">>),
  {ok, _Conn} = barrel_httpc:connect(<<"http://localhost:7080/dbs/testdb2">>),
  ok = barrel_httpc:delete_database(<<"http://localhost:7080/dbs/testdb2">>),
  {error, not_found} = barrel_httpc:connect(<<"http://localhost:7080/dbs/testdb2">>),
  [<<"source">>, <<"testdb">>] = barrel_httpc:database_names(<<"http://localhost:7080">>),
  DbUrl = <<"http://localhost:7080/dbs/testdb2">>,
  {ok, DbUrl} = barrel_httpc:create_database(
    <<"http://localhost:7080">>, #{ <<"database_id">> => <<"testdb2">>}
  ),
  [<<"source">>, <<"testdb">>, <<"testdb2">>] = barrel_httpc:database_names(<<"http://localhost:7080">>),
  ok = barrel_httpc:delete_database(<<"http://localhost:7080/dbs/testdb2">>),
  ok.

basic_op(Config) ->
  {error, not_found} = barrel_httpc:get(db(Config), <<"a">>, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_httpc:put(db(Config), Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_httpc:get(db(Config), <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_httpc:delete(db(Config), <<"a">>, RevId, []),
  {error, not_found} = barrel_httpc:get(db(Config), <<"a">>, []).

update_doc(Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_httpc:put(db(Config), Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_httpc:get(db(Config), <<"a">>, []),
  Doc3 = Doc2#{ <<"v">> => 2},
  {ok, <<"a">>, RevId2} = barrel_httpc:put(db(Config), Doc3, []),
  true = (RevId =/= RevId2),
  Doc4 = Doc3#{<<"_rev">> => RevId2},
  {ok, Doc4} = barrel_httpc:get(db(Config), <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_httpc:delete(db(Config), <<"a">>, RevId2, []),
  {error, not_found} = barrel_httpc:get(db(Config), <<"a">>, []),
  {ok, <<"a">>, _RevId3} = barrel_httpc:put(db(Config), Doc, []).

bad_doc(Config) ->
  Doc = #{ <<"v">> => 1},
  try barrel_httpc:put(db(Config), Doc, [])
  catch
    error:{bad_doc, invalid_docid} -> ok
  end.

create_doc(Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_httpc:post(db(Config), Doc, []),
  CreatedDoc = Doc#{ <<"id">> => DocId, <<"_rev">> => RevId},
  {ok, CreatedDoc} = barrel_httpc:get(db(Config), DocId, []),
  {error, not_found} = barrel_httpc:post(db(Config), CreatedDoc, []),
  Doc2 = #{<<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_httpc:post(db(Config), Doc2, []).

get_revisions(Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_httpc:post(db(Config), Doc, []),
  {ok, Doc2} = barrel_httpc:get(db(Config), DocId, []),
  Doc3 = Doc2#{ v => 2},
  {ok, DocId, RevId2} = barrel_httpc:put(db(Config), Doc3, []),
  {ok, Doc4} = barrel_httpc:get(db(Config), DocId, [{history, true}]),
  Revisions = parse_revisions(Doc4),
  Revisions == [RevId2, RevId].

put_rev(Config) ->
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} = barrel_httpc:post(db(Config), Doc, []),
  {ok, Doc2} = barrel_httpc:get(db(Config), DocId, []),
  Doc3 = Doc2#{ <<"v">> => 2},
  {ok, DocId, RevId2} = barrel_httpc:put(db(Config), Doc3, []),
  Doc4_0 = Doc2#{ <<"v">> => 3 },
  {Pos, _} = parse_revision(RevId),
  NewRev = revid(Pos +1, RevId, Doc4_0),
  Doc4 = Doc4_0#{<<"_rev">> => NewRev},
  io:format("doc is ~p~n", [Doc4]),
  History = [NewRev, RevId],
  {ok, DocId, _RevId3} = barrel_httpc:put_rev(db(Config), Doc4, History, []),
  {ok, Doc5} = barrel_httpc:get(db(Config), DocId, [{history, true}]),
  Revisions = parse_revisions(Doc5),
  Revisions == [RevId2, RevId].

revsdiff(Config) ->
  Doc = #{ <<"id">> => <<"revsdiff">>, <<"v">> => 1},
  {ok, <<"revsdiff">>, RevId} = barrel_httpc:put(db(Config), Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId, <<"v">> => 2},
  {ok, <<"revsdiff">>, _RevId3} = barrel_httpc:put(db(Config), Doc2, []),
  {ok, [<<"1-missing">>], []} = barrel_httpc:revsdiff(db(Config), <<"revsdiff">>, [<<"1-missing">>]),
  ok.

fold_by_id(Config) ->
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _RevId} = barrel_httpc:put(db(Config), Doc, []),
  Doc2 = #{ <<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} = barrel_httpc:put(db(Config), Doc2, []),
  Doc3 = #{ <<"id">> => <<"c">>, <<"v">> => 1},
  {ok, <<"c">>, _RevId3} = barrel_httpc:put(db(Config), Doc3, []),
  Fun = fun
          (#{ <<"id">> := DocId}, Acc1) ->
            {ok, [DocId | Acc1]}
        end,
  {ok, Acc} = barrel_httpc:fold_by_id(db(Config), Fun, [], []),
  [<<"c">>, <<"b">>, <<"a">>] = Acc,
  {ok, Acc2} = barrel_httpc:fold_by_id(db(Config), Fun, [], [{lt, <<"b">>}]),
  [<<"a">>] = Acc2,
  {ok, Acc3} = barrel_httpc:fold_by_id(db(Config), Fun, [], [{lte, <<"b">>}]),
  [<<"b">>, <<"a">>] = Acc3,
  {ok, Acc4} = barrel_httpc:fold_by_id(db(Config), Fun, [], [{gte, <<"b">>}]),
  [<<"c">>, <<"b">>] = Acc4,
  {ok, Acc5} = barrel_httpc:fold_by_id(db(Config), Fun, [], [{gt, <<"b">>}]),
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
  {ok, <<"AndersenFamily">>, _Rev} = barrel_httpc:put(db(Config), Doc, []),
  timer:sleep(400),
  {ok, Doc1} = barrel_httpc:get(db(Config), <<"AndersenFamily">>, []),
  Fun = fun(Obj, Acc) -> {ok, [Obj| Acc]} end,
                                           {ok,
    [#{<<"id">> := <<"AndersenFamily">>,
      <<"val">> := <<"AndersenFamily">>}]} = barrel_httpc:fold_by_path(db(Config), <<"id">>, Fun, [], []),
  {ok, [Obj]} = barrel_httpc:fold_by_path(db(Config), <<"id">>, Fun, [], [{include_docs, true}] ),
  #{<<"doc">> := Doc1} = Obj,
  ok.

multiple_put(Config) ->
  Self = self(),
  Pids  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Doc = #{ <<"id">> => DocId, <<"val">> => I},
      Pid = spawn_link(
        fun() ->
          {ok, DocId, _} = barrel_httpc:put(db(Config), Doc, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids),
  #{ <<"docs_count">> := 500 } = barrel_httpc:database_infos(?DB_URL),
  ok.


multiple_get(Config) ->
  Self = self(),
  Pids  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Doc = #{ <<"id">> => DocId, <<"val">> => I},
      Pid = spawn_link(
        fun() ->
          {ok, DocId, _} = barrel_httpc:put(db(Config), Doc, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids),
  #{ <<"docs_count">> := 500 } = barrel_httpc:database_infos(?DB_URL),
  Pids1  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Pid = spawn_link(
        fun() ->
          {ok, #{ <<"id">> := DocId }} = barrel_httpc:get(db(Config), DocId, []),
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
          {ok, DocId, _} = barrel_httpc:put(db(Config), Doc, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids),
  #{ <<"docs_count">> := 500 } = barrel_httpc:database_infos(?DB_URL),
  Pids1  = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Pid = spawn_link(
        fun() ->
          {ok, #{ <<"id">> := DocId, <<"_rev">> := Rev }} = barrel_httpc:get(db(Config), DocId, []),
          {ok, _, _} = barrel_httpc:delete(db(Config), DocId, Rev, []),
          Self ! {ok, self()}
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, 500)
  ),
  ok = wait_pids(Pids1),
  #{ <<"docs_count">> := 0 } = barrel_httpc:database_infos(?DB_URL),
  ok.


change_since(Config) ->
  Fun = fun(Change, Acc) ->
    Id = maps:get(<<"id">>, Change),
    {ok, [Id|Acc]}
        end,
  {ok, []} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:put(db(Config), Doc, []),
  {ok, [<<"aa">>]} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_httpc:put(db(Config), Doc2, []),
  {ok, _} = barrel_httpc:get(db(Config), <<"bb">>, []),
  {ok, [<<"bb">>, <<"aa">>]} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  {ok, [<<"bb">>]} = barrel_httpc:changes_since(db(Config), 1, Fun, [], []),
  {ok, []} = barrel_httpc:changes_since(db(Config), 2, Fun, [], []),
  Doc3 = #{ <<"id">> => <<"cc">>, <<"v">> => 1},
  {ok, <<"cc">>, _RevId3} = barrel_httpc:put(db(Config), Doc3, []),
  {ok, [<<"cc">>]} = barrel_httpc:changes_since(db(Config), 2, Fun, [], []),
  ok.

change_deleted(Config) ->
  Fun = fun(Change, Acc) ->
    Id = maps:get(<<"id">>, Change),
    Del = maps:get(<<"deleted">>, Change, false),
    {ok, [{Id, Del}|Acc]}
        end,
  {ok, []} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:put(db(Config), Doc, []),
  {ok, [{<<"aa">>, false}]} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, RevId2} = barrel_httpc:put(db(Config), Doc2, []),
  {ok, _} = barrel_httpc:get(db(Config), <<"bb">>, []),
  {ok, [{<<"bb">>, false}, {<<"aa">>, false}]} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  {ok, [{<<"bb">>, false}]} = barrel_httpc:changes_since(db(Config), 1, Fun, [], []),
  {ok, <<"bb">>, _} = barrel_httpc:delete(db(Config), <<"bb">>, RevId2, []),
  {ok, [{<<"bb">>, true}]} = barrel_httpc:changes_since(db(Config), 2, Fun, [], []),
  {ok, [{<<"bb">>, true}, {<<"aa">>, false}]} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  ok.



change_since_include_doc(Config) ->
  Fun =
  fun(Change, Acc) ->
    Seq = maps:get(<<"seq">>, Change),
    {ok, [{Seq, maps:get(<<"doc">>, Change)} |Acc]}
  end,
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:put(db(Config), Doc, []),
  {ok, Doc1} = barrel_httpc:get(db(Config), <<"aa">>, []),
  {ok, [Change]} = barrel_httpc:changes_since(db(Config), 0, Fun, [], [{<<"include_doc">>, <<"true">>}]),
  {1, Doc1} = Change,
  ok.

change_since_many(Config) ->
  Fun = fun(Change, Acc) ->
          Seq = maps:get(<<"seq">>, Change),
          {ok, [{Seq, Change}|Acc]}
        end,
  
  %% No changes. Database is empty.
  {ok, []} = barrel_httpc:changes_since(db(Config), 0, Fun, [], []),
  
  %% Add 20 docs (doc1 to doc20).
  AddDoc = fun(N) ->
              K = integer_to_binary(N),
              Key = <<"doc", K/binary>>,
              Doc = #{ <<"id">> => Key, <<"v">> => 1},
    {ok, Key, _RevId} = barrel_httpc:put(db(Config), Doc, [])
           end,
  [AddDoc(N) || N <- lists:seq(1,20)],
  
  %% Delete doc1
  {ok, Doc1} = barrel_httpc:get(db(Config), <<"doc1">>, []),
  #{<<"_rev">> := RevId} = Doc1,
  {ok, <<"doc1">>, _} = barrel_httpc:delete(db(Config), <<"doc1">>, RevId, []),
  
  %% 20 changes (for doc1 to doc20)
  {ok, All} = barrel_httpc:changes_since(db(Config), 0, Fun, [], [{history, all}]),
  20 = length(All),
  %% History for doc1 includes creation and deletion
  {21, #{<<"changes">> := HistoryDoc1}} = hd(All),
  2 = length(HistoryDoc1),
  
  {ok, [{21, #{<<"id">> := <<"doc1">>}},
        {20, #{<<"id">> := <<"doc20">>}},
        {19, #{<<"id">> := <<"doc19">>}}]} = barrel_httpc:changes_since(db(Config), 18, Fun, [], []),
  {ok, [{21, #{<<"id">> := <<"doc1">>}},
        {20, #{<<"id">> := <<"doc20">>}}]} = barrel_httpc:changes_since(db(Config), 19, Fun, [], []),
  {ok, []} = barrel_httpc:changes_since(db(Config), 21, Fun, [], []),
  ok.

system_docs(_Config) ->
  Doc = #{<<"v">> => 1},
  ok = barrel_httpc:put_system_doc(<<"testdb">>, <<"a">>, Doc),
  {ok, Doc} = barrel_db:get_system_doc(<<"testdb">>, <<"a">>),
  ok = barrel_db:delete_system_doc(<<"testdb">>, <<"a">>),
  {error, not_found} = barrel_db:get_system_doc(<<"testdb">>, <<"a">>),
  ok.


%% internal

wait_pids([]) -> ok;
wait_pids(Pids) ->
  receive
    {ok, Pid} -> wait_pids(Pids -- [Pid])
  after 5000 -> {error, receive_pids}
  end.


%% from barrel_doc in barrel_commons

parse_revision(<<"">>) -> {0, <<"">>};
parse_revision(Rev) when is_binary(Rev) ->
  case binary:split(Rev, <<"-">>) of
    [BinPos, Hash] -> {binary_to_integer(BinPos), Hash};
    _ -> error(bad_rev)
  end;
parse_revision(Rev) when is_list(Rev) -> parse_revision(list_to_binary(Rev));
parse_revision(Rev) -> error({bad_rev, Rev}).

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

revid(Pos, Parent, Body0) ->
  Ctx0 = crypto:hash_init(md5),
  Body = maps:filter(fun
                       (<<"">>, _) -> false;
                       (<<"_deleted">> , _) -> true;
                       (<<"_", _/binary>>, _) -> false;
                       (_, _) -> true
                     end, Body0),
  BinPos = integer_to_binary(Pos),
  Ctx2 = lists:foldl(fun(V, C) ->
    crypto:hash_update(C, V)
                     end, Ctx0, [BinPos, Parent, term_to_binary(Body)]),
  Digest = crypto:hash_final(Ctx2),
  << BinPos/binary, "-", (barrel_httpc_lib:to_hex(Digest))/binary >>.