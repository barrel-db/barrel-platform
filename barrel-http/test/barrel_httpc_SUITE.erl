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
        , changes_since_history/1
        , db_updated/1
        , system_doc/1
        ]).

all() -> [ info_database
         , create_doc
         , put_get_delete
         , put_rev
         , changes_since
         , changes_since_history
         , db_updated
         , system_doc
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  Config.

url() ->
  <<"http://localhost:8080/testdb/testdb">>.

init_per_testcase(_, Config) ->
  {true, Conn} = barrel:create_database(testdb, <<"testdb">>),
  {ok, HttpConn} = barrel_httpc:start_link(url(), []),
  [{http_conn, HttpConn}, {conn, Conn}|Config].

end_per_testcase(_, Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  Conn = proplists:get_value(conn, Config),
  ok = barrel_httpc:disconnect(HttpConn),
  ok = barrel:delete_database(Conn),
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
  CreatedDoc = Doc#{ <<"id">> => DocId, <<"_rev">> => RevId},
  {ok, CreatedDoc} = barrel_httpc:get(HttpConn, DocId, []),
  {error, not_found} =  barrel_httpc:post(HttpConn, CreatedDoc, []),
  Doc2 = #{<<"id">> => <<"b">>, <<"v">> => 1},
  {ok, <<"b">>, _RevId2} =  barrel_httpc:post(HttpConn, Doc2, []).


put_get_delete(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  {error, not_found} = barrel_httpc:get(HttpConn, <<"a">>, []),
  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_httpc:put(HttpConn, <<"a">>, Doc, []),
  Doc2 = Doc#{<<"_rev">> => RevId},
  {ok, Doc2} = barrel_httpc:get(HttpConn, <<"a">>, []),
  {ok, <<"a">>, _RevId2} = barrel_httpc:delete(HttpConn, <<"a">>, RevId, []),
  {ok, DeletedDoc} = barrel_httpc:get(HttpConn, <<"a">>, []),
  true = maps:get(<<"_deleted">>, DeletedDoc).

put_rev(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, RevId} = barrel_httpc:put(HttpConn, DocId, Doc, []),
  {ok, Doc2} = barrel_httpc:get(HttpConn, DocId, []),
  Doc3 = Doc2#{ v => 2 },
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc3),
  Doc4 = Doc3#{<<"_rev">> => NewRev},
  History = [NewRev, RevId],
  {ok, DocId, NewRev} = barrel_httpc:put_rev(HttpConn, DocId, Doc4, History, []),
  ok.

system_doc(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  DocId = <<"a">>,
  Doc = #{<<"v">> => 1},
  ok = barrel_httpc:write_system_doc(HttpConn, DocId, Doc),
  {ok, Doc} = barrel_httpc:read_system_doc(HttpConn, DocId),
  ok = barrel_httpc:delete_system_doc(HttpConn, DocId),
  {error, not_found} = barrel_httpc:read_system_doc(HttpConn, DocId),
  ok.

changes_since(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:put(HttpConn, <<"aa">>, Doc, []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_httpc:put(HttpConn, <<"bb">>, Doc2, []),

  [{2, #{id := <<"bb">>}}, {1, #{id := <<"aa">>}}] = since(HttpConn, 0),
  [{2, #{id := <<"bb">>}}] = since(HttpConn, 1),
  [] = since(HttpConn, 2),

  Doc3 = #{ <<"id">> => <<"cc">>, <<"v">> => 1},
  {ok, <<"cc">>, _RevId3} = barrel_httpc:put(HttpConn, <<"cc">>, Doc3, []),
  [{3, #{id := <<"cc">>}},{2, #{id := <<"bb">>}}] = since(HttpConn, 1),
  ok.

changes_since_history(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, AARevId} = barrel_httpc:put(HttpConn, <<"aa">>, Doc, []),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, BBRevId} = barrel_httpc:put(HttpConn, <<"bb">>, Doc2, []),
  {ok, <<"aa">>, DeleteAARevid} = barrel_httpc:delete(HttpConn, <<"aa">>, AARevId, []),
  [{3,
    #{changes := [DeleteAARevid, AARevId],
      deleted := true,
      id := <<"aa">>,
      seq := 3}},
   {2,
    #{changes := [BBRevId],
      id := <<"bb">>,
      seq := 2}}] = since(HttpConn, 0, [{history, all}]).

db_updated(Config) ->
  HttpConn = proplists:get_value(http_conn, Config),

  ok = barrel_event:reg({barrel_httpc, HttpConn}),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:put(HttpConn, <<"aa">>, Doc, []),

  Msg = one_msg(),
  {'$barrel_event', {barrel_httpc, HttpConn}, db_updated} = Msg,
  true = queue_is_empty(),

  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _RevId2} = barrel_httpc:put(HttpConn, <<"bb">>, Doc2, []),

  Msg = one_msg(),
  {'$barrel_event', {barrel_httpc, HttpConn}, db_updated} = Msg,
  true = queue_is_empty(),

  ok = barrel_event:unreg(),
  ok.


since(HttpConn, Since) ->
  since(HttpConn, Since, []).

since(HttpConn, Since, Options) ->
  Fun = fun(Seq, Change, Acc) ->
            {ok, [{Seq, Change}|Acc]}
        end,
  barrel_httpc:changes_since(HttpConn, Since, Fun, [], Options).


one_msg() ->
  receive
    M -> M
  after 2000 ->
      {error, timeout}
  end.

queue_is_empty() ->
  {message_queue_len, 0} = erlang:process_info(self(), message_queue_len),
  true.
