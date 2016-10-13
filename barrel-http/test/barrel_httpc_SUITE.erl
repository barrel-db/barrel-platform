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

-module(barrel_httpc_SUITE).

-export([ all/0
        , end_per_suite/1
        , end_per_testcase/2
        , init_per_suite/1
        , init_per_testcase/2
        ]).

-export([ info_database/1
        , create_doc/1
        , put_get_delete/1
        , put_rev/1
        , changes_since/1
        , db_updated/1
        ]).

all() -> [ info_database
         , create_doc
         , put_get_delete
         , put_rev
         , changes_since
         , db_updated
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  Config.

url() ->
  <<"http://localhost:8080/testdb">>.

init_per_testcase(_, Config) ->
  ok = barrel_db:start(<<"testdb">>, barrel_test_rocksdb),
  {ok, HttpConn} = barrel_httpc:connect(url(), []),
  [{http_conn, HttpConn}|Config].

end_per_testcase(_, Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  stopped = barrel_httpc:stop(HttpConn),
  ok = barrel_db:clean(<<"testdb">>),
  ok.

end_per_suite(Config) ->
  catch erocksdb:destroy(<<"testdb">>),
  Config.

%% ----------

info_database(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  {ok, Info} = barrel_httpc:infos(HttpConn),
  <<"testdb">> = maps:get(<<"name">>, Info),
  ok.

create_doc(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  Doc = #{<<"v">> => 1},
  {ok, DocId, RevId} =  barrel_httpc:post(HttpConn, Doc, []),
  CreatedDoc = Doc#{ <<"_id">> => DocId, <<"_rev">> => RevId},
  {ok, CreatedDoc} = barrel_httpc:get(HttpConn, DocId, []),
  {error, not_found} =  barrel_httpc:post(HttpConn, CreatedDoc, []),
  Doc2 = #{<<"_id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} =  barrel_httpc:post(HttpConn, Doc2, []).


put_get_delete(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  {error, not_found} = barrel_httpc:get(HttpConn, <<"a">>, []),
  Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_httpc:put(HttpConn, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_httpc:get(HttpConn, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_httpc:delete(HttpConn, <<"a">>, RevId, []),
  {ok, DeletedDoc} = barrel_httpc:get(HttpConn, <<"a">>, []),
  true = maps:get(<<"_deleted">>, DeletedDoc).

put_rev(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  DocId = <<"a">>,
  Doc = #{ <<"_id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_httpc:put(HttpConn, DocId, Doc, []),
  {ok, Doc2} = barrel_httpc:get(HttpConn, DocId, []),
  Doc3 = Doc2#{ v => 2 },
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc3),
  Doc4 = Doc3#{<<"_rev">> => NewRev},
  History = [NewRev, RevId],
  {ok, DocId, NewRev} = barrel_httpc:put_rev(HttpConn, DocId, Doc4, History, []),
  ok.

changes_since(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  Doc = #{ <<"_id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:put(HttpConn, <<"aa">>, Doc, []),
  Doc2 = #{ <<"_id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_httpc:put(HttpConn, <<"bb">>, Doc2, []),

  [{2, <<"bb">>}, {1, <<"aa">>}] = since(HttpConn, 0),
                 [{2, <<"bb">>}] = since(HttpConn, 1),
                              [] = since(HttpConn, 2),

  {ok, <<"cc">>, _RevId2} = barrel_httpc:put(HttpConn, <<"cc">>, Doc2, []),

                 [{3, <<"cc">>}] = since(HttpConn, 2).

db_updated(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),

  ok = barrel_event:reg(HttpConn),
  Doc = #{ <<"_id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:put(HttpConn, <<"aa">>, Doc, []),

  Msg = one_msg(),
  {'$barrel_event', HttpConn, db_updated} = Msg,
  true = queue_is_empty(),

  Doc2 = #{ <<"_id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_httpc:put(HttpConn, <<"bb">>, Doc2, []),

  Msg = one_msg(),
  {'$barrel_event', HttpConn, db_updated} = Msg,
  true = queue_is_empty(),

  ok = barrel_event:unreg(),
  ok.


since(HttpConn, Since) ->
  Fun = fun(Seq, DocInfo, Doc, Acc) ->
            {error, doc_not_fetched} = Doc,
            Id = maps:get(id, DocInfo),
            {ok, [{Seq, Id}|Acc]}
        end,
  barrel_httpc:changes_since(HttpConn, Since, Fun, []).


one_msg() ->
  receive
    M -> M
  after 2000 ->
      {error, timeout}
  end.

queue_is_empty() ->
  {message_queue_len, 0} = erlang:process_info(self(), message_queue_len),
  true.
