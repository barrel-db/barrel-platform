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

-module(barrel_rest_db_SUITE).

-export([all/0,
         end_per_suite/1,
         end_per_testcase/2,
         init_per_suite/1,
         init_per_testcase/2]).

-export([ accept_get/1
        , accept_put/1
        , accept_delete/1
        , reject_store_unknown/1
        , reject_db_unknown/1
        ]).

all() -> [ accept_get
         , accept_put
         , accept_delete
         , reject_store_unknown
         , reject_db_unknown
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


accept_get(_Config) ->
  {200, R1} = test_lib:req(get, "/testdb/testdb"),
  A1 = jsx:decode(R1, [return_maps]),
  <<"testdb">> = maps:get(<<"name">>, A1),
  %% TODO refactor
  %% {404, _} = test_lib:req(get, "/unknwondb"),
  ok.

accept_put(_Config) ->
  Cat = "{\"_id\": \"cat\", \"name\" : \"tom\"}",
  {400, _} = test_lib:req(put, "/testdb/newdb/cat", Cat),
  {201, _} = test_lib:req(put, "/testdb/newdb", []),
  {201, _} = test_lib:req(put, "/testdb/newdb/cat", Cat),
  ok.

accept_delete(_Config) ->
  false = lists:member(<<"tobedeleted">>, barrel:database_names(testdb)),
  {true, _} = barrel:create_database(testdb, <<"tobedeleted">>),
  true = lists:member(<<"tobedeleted">>, barrel:database_names(testdb)),
  {200, _} = test_lib:req(delete, "/testdb/tobedeleted"),
  false = lists:member(<<"tobedeleted">>, barrel:database_names(testdb)),
  ok.

reject_store_unknown(_Config) ->
  {400, _} = test_lib:req(get, "/badstore/testdb"),
  {400, _} = test_lib:req(put, "/badstore/testdb"),
  ok.

reject_db_unknown(_Config) ->
  {404, _} = test_lib:req(get, "/testdb/doesnotexist"),
  ok.
