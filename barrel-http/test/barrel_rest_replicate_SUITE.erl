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

-module(barrel_rest_replicate_SUITE).

-export([all/0,
         end_per_suite/1,
         end_per_testcase/2,
         init_per_suite/1,
         init_per_testcase/2]).

-export([ accept_post_get/1
        , accept_put_get/1
        , accept_delete/1
        , reject_replication_name_unknown/1
        , reject_store_or_db_unknown/1
        , reject_bad_json /1
        ]).

all() -> [ accept_post_get
         , accept_put_get
         , accept_delete
         , reject_replication_name_unknown
         , reject_store_or_db_unknown
         , reject_bad_json
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


accept_post_get(_Config) ->
  %% create 2 databases
  {201, _} = test_lib:req(put, "/testdb/dba", []),
  {201, _} = test_lib:req(put, "/testdb/dbb", []),

  {404, _} = test_lib:req(get, "/testdb/dbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"http://localhost:8080/testdb/dba">>,
              <<"target">> => <<"http://localhost:8080/testdb/dbb">>},
  {200, R} = test_lib:req(post, "/_replicate", Request),
  #{<<"name">> := NameBin} = jsx:decode(R, [return_maps]),
  Name = binary_to_list(NameBin),

  %% put one doc in source db
  Mouse = "{\"id\": \"mouse\", \"name\" : \"jerry\"}",
  {201, _} = test_lib:req(put, "/testdb/dba/mouse", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = test_lib:req(get, "/testdb/dbb/mouse"),

  {404, _} = test_lib:req(get, "/_replicate/doesnotexist"),
  {200, R2} = test_lib:req(get, "/_replicate/" ++ Name),
  Metrics = jsx:decode(R2, [return_maps]),
  #{<<"docs_read">> := 1,
    <<"docs_written">> := 1} = Metrics,
  ok.

accept_put_get(_Config) ->
  %% create 2 databases
  {201, _} = test_lib:req(put, "/testdb/dbaa", []),
  {201, _} = test_lib:req(put, "/testdb/dbbb", []),

  {404, _} = test_lib:req(get, "/testdb/dbbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"http://localhost:8080/testdb/dbaa">>,
              <<"target">> => <<"http://localhost:8080/testdb/dbbb">>,
              <<"persisted">> => true},
  {200, R} = test_lib:req(put, "/_replicate/myreplication", Request),
  #{<<"name">> := <<"myreplication">>} = jsx:decode(R, [return_maps]),

  %% put one doc in source db
  Mouse = "{\"id\": \"mouse\", \"name\" : \"jerry\"}",
  {201, _} = test_lib:req(put, "/testdb/dbaa/mouse", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = test_lib:req(get, "/testdb/dbbb/mouse"),

  {200, R2} = test_lib:req(get, "/_replicate/myreplication"),
  Metrics = jsx:decode(R2, [return_maps]),
  #{<<"docs_read">> := 1,
    <<"docs_written">> := 1} = Metrics,
  ok.

accept_delete(_Config) ->
  %% create 2 databases
  {201, _} = test_lib:req(put, "/testdb/dbaaa", []),
  {201, _} = test_lib:req(put, "/testdb/dbbbb", []),

  {404, _} = test_lib:req(get, "/testdb/dbbbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"http://localhost:8080/testdb/dbaaa">>,
              <<"target">> => <<"http://localhost:8080/testdb/dbbbb">>,
              <<"persisted">> => true},
  {200, R} = test_lib:req(put, "/_replicate/tasktobedeleted", Request),
  #{<<"name">> := <<"tasktobedeleted">>} = jsx:decode(R, [return_maps]),

  %% put one doc in source db
  Mouse = "{\"id\": \"mouse\", \"name\" : \"jerry\"}",
  {201, _} = test_lib:req(put, "/testdb/dbaaa/mouse", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = test_lib:req(get, "/testdb/dbbbb/mouse"),

  %% delete the replication task
  {200, _} = test_lib:req(get, "/_replicate/tasktobedeleted"),
  {200, _} = test_lib:req(delete, "/_replicate/tasktobedeleted"),
  {404, _} = test_lib:req(get, "/_replicate/tasktobedeleted"),

  %% put another doc in source db
  Cat = "{\"id\": \"cat\", \"name\" : \"tom\"}",
  {201, _} = test_lib:req(put, "/testdb/dbaaa/cat", Cat),
  timer:sleep(500),

  %% it has not been replicated
  {404, _} = test_lib:req(get, "/testdb/dbbbb/cat"),
  ok.


reject_replication_name_unknown(_Config) ->
  {404, _} = test_lib:req(get, "/_replicate/unknown"),
  {404, _} = test_lib:req(delete, "/_replicate/unknown"),
  ok.

reject_store_or_db_unknown(_Config) ->
  M = #{<<"persisted">> => true},
  NoStoreSource = M#{<<"source">> => <<"http://localhost:8080/nostore/dba">>,
                    <<"target">> => <<"http://localhost:8080/testdb/dbb">>},
  NoStoreTarget = M#{<<"source">> => <<"http://localhost:8080/testdb/dba">>,
                    <<"target">> => <<"http://localhost:8080/nostore/dbb">>},
  NoDbSource = M#{<<"source">> => <<"http://localhost:8080/testdb/nodb">>,
                 <<"target">> => <<"http://localhost:8080/testdb/dbb">>},
  NoDbTarget = M#{<<"source">> => <<"http://localhost:8080/testdb/dba">>,
                 <<"target">> => <<"http://localhost:8080/testdb/nodb">>},

  {400, _} = test_lib:req(post, "/_replicate", NoStoreSource),
  {400, _} = test_lib:req(post, "/_replicate", NoStoreTarget),
  {400, _} = test_lib:req(post, "/_replicate", NoDbSource),
  {400, _} = test_lib:req(post, "/_replicate", NoDbTarget),
  ok.

reject_bad_json(_Config) ->
  BadJson = "{\"source\": \"badjson no complet",
  NoSource = #{<<"target">> => <<"http://localhost:8080/nostore/dbb">>},
  NoTarget = #{<<"source">> => <<"http://localhost:8080/nostore/dba">>},

  {400, _} = test_lib:req(post, "/_replicate", BadJson),
  {400, _} = test_lib:req(post, "/_replicate", NoSource),
  {400, _} = test_lib:req(post, "/_replicate", NoTarget),
  ok.
