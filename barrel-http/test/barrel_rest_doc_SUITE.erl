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

-module(barrel_rest_doc_SUITE).

-export([ all/0
        , end_per_suite/1
        , end_per_testcase/2
        , init_per_suite/1
        , init_per_testcase/2
        ]).

-export([ accept_get/1
        , accept_post/1
        , accept_put/1
        , accept_delete/1
        , reject_store_or_db_unknown/1
        , reject_unknown_query_parameters/1
        , reject_bad_json/1
        , two_databases_on_same_store/1
        , revsdiff/1
        , put_rev/1
        ]).

all() -> [ accept_get
         , accept_post
         , accept_put
         , accept_delete
         , reject_store_or_db_unknown
         , reject_unknown_query_parameters
         , reject_bad_json
         , two_databases_on_same_store
         , revsdiff
         , put_rev
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  Config.

init_per_testcase(_, Config) ->
  {true, Conn} = barrel:create_database(testdb, <<"testdb">>),
  [{conn, Conn} |Config].

end_per_testcase(_, Config) ->
  Conn = proplists:get_value(conn, Config),
  ok = barrel:delete_database(Conn),
  Config.

end_per_suite(Config) ->
  catch erocksdb:destroy(<<"testdb">>), Config.


accept_get(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{<<"name">> => <<"tom">>},
  {ok, _, _} = barrel:put(Conn, <<"acceptget">>, Doc, []),

  {200, R} = test_lib:req(get, "/testdb/testdb/acceptget"),
  J = jsx:decode(R, [return_maps]),
  #{<<"name">> := <<"tom">>} = J,
  ok.

accept_post(Config) ->
  Conn = proplists:get_value(conn, Config),
  D1 = #{<<"name">> => <<"tom">>},
  {200, R} = test_lib:req(post, "/testdb/testdb", D1),

  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  {ok, Doc} = barrel:get(Conn, DocId, []),
  #{<<"name">> := <<"tom">>} = Doc,
  ok.

accept_put(Config) ->
  Conn = proplists:get_value(conn, Config),
  D1 = #{<<"id">> => <<"tom">>, <<"name">> => <<"tom">>},
  {201, R} = test_lib:req(put, "/testdb/testdb/cat", D1),

  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  {ok, Doc} = barrel:get(Conn, DocId, []),
  #{<<"name">> := <<"tom">>} = Doc,
  ok.

accept_delete(Config) ->
  Conn = proplists:get_value(conn, Config),
  Doc = #{<<"name">> => <<"tom">>},
  {ok, _, RevIdBin} = barrel:put(Conn, <<"acceptdelete">>, Doc, []),
  RevId = binary_to_list(RevIdBin),

  Url = "/testdb/testdb/acceptdelete?rev=" ++ RevId,
  {200, _} = test_lib:req(delete, Url),

  {ok, DeletedDoc} = barrel:get(Conn, <<"acceptdelete">>, []),
  #{<<"_deleted">> := true} = DeletedDoc,
  ok.

reject_store_or_db_unknown(_) ->
  Doc = #{<<"name">> => <<"tom">>},
  {400, _} = test_lib:req(get, "/bad/testdb/docid"),
  {400, _} = test_lib:req(put, "/bad/testdb/docid", Doc),
  %% {400, _} = test_lib:req(post, "/bad/testdb", Doc),
  {400, _} = test_lib:req(delete, "/bad/testdb/docid"),

  {400, _} = test_lib:req(get, "/testdb/bad/docid"),
  {400, _} = test_lib:req(put, "/testdb/bad/docid", Doc),
  %% {400, _} = test_lib:req(post, "/testdb/bad", Doc),
  {400, _} = test_lib:req(delete, "/testdb/bad/docid"),
  ok.

reject_unknown_query_parameters(_) ->
  Doc = #{<<"name">> => <<"tom">>},
  {400, _} = test_lib:req(get, "/testdb/testdb/docid?badparam=whatever"),
  {400, _} = test_lib:req(put, "/testdb/testdb/docid?badparam=whatever", Doc),
  %% {400, _} = test_lib:req(post, "/testdb/testdb?badparam=whatever", Doc),
  {400, _} = test_lib:req(delete, "/testdb/testdb/docid?badparam=whatever"),
  ok.

reject_bad_json(_) ->
  Doc = "{\"name\": \"badjson",
  {400, _} = test_lib:req(put, "/testdb/testdb/docid", Doc),
  %% {400, _} = test_lib:req(post, "/testdb/testdb", Doc),
  ok.


put_cat() ->
  Doc = "{\"_id\": \"cat\", \"name\" : \"tom\"}",
  {201, R} = test_lib:req(put, "/testdb/testdb/cat", Doc),
  J = jsx:decode(R, [return_maps]),
  binary_to_list(maps:get(<<"rev">>, J)).

two_databases_on_same_store(_Config) ->
  Cat = "{\"_id\": \"cat\", \"name\" : \"tom\"}",
  Dog = "{\"_id\": \"dog\", \"name\": \"spike\"}",
  {201, _} = test_lib:req(put, "/testdb/db1", []),
  {201, _} = test_lib:req(put, "/testdb/db2", []),
  {201, _} = test_lib:req(put, "/testdb/db1/cat", Cat),
  {201, _} = test_lib:req(put, "/testdb/db2/dog", Dog),
  {200, _} = test_lib:req(get, <<"/testdb/db1/cat">>),
  {404, _} = test_lib:req(get, <<"/testdb/db2/cat">>),
  {200, _} = test_lib:req(get, <<"/testdb/db2/dog">>),
  {404, _} = test_lib:req(get, <<"/testdb/db1/dog">>),
  ok.

revsdiff(_Config) ->
  CatRevId = put_cat(),
  Request = #{<<"cat">> => [CatRevId, <<"2-missing">>]},
  {200, R} = test_lib:req(post, "/testdb/testdb/_revs_diff", Request),
  A = jsx:decode(R, [return_maps]),
  CatDiffs = maps:get(<<"cat">>, A),
  Missing = maps:get(<<"missing">>, CatDiffs),
  true = lists:member(<<"2-missing">>, Missing),
  ok.

put_rev(Config) ->
  Conn = proplists:get_value(conn, Config),
  RevId = put_cat(),
  {ok, Doc} = barrel:get(Conn, <<"cat">>, []),
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc),
  History = [NewRev, RevId],
  Request = #{<<"document">> => Doc,
              <<"history">> => History},
  {201, R} = test_lib:req(put, "/testdb/testdb/cat?edit=true", Request),
  A = jsx:decode(R, [return_maps]),
  true = maps:get(<<"ok">>, A),
  ok.

