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

-module(barrel_rest_dbs_SUITE).

-export(
   [ all/0
   , end_per_suite/1
   , init_per_suite/1
   , init_per_testcase/2
   , end_per_testcase/2
   ]).

-export(
   [ db_info/1
   , accept_post/1
   , reject_bad_json/1
   , reject_unknown_database/1
   , dbs/1
   ]).

all() ->
  [ db_info
  , accept_post
  , reject_bad_json
  , reject_unknown_database
  , dbs
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_local:create_db(<<"testdb">>, #{}),
  _ = barrel_local:create_db(<<"source">>, #{}),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel_local:delete_db(<<"testdb">>),
  ok = barrel_local:delete_db(<<"source">>),
  Config.

end_per_suite(Config) ->
  application:stop(barrel),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.

db_info(_Config) ->
  {200, R1} = test_lib:req(get, "/dbs/testdb"),
  Info = jsx:decode(R1, [return_maps]),

  #{<<"name">> := _,
    <<"id">> := _,
    <<"docs_count">> := _,
    <<"last_update_seq">> := _,
    <<"system_docs_count">> := _,
    <<"last_index_seq">> := _} = Info,
  ok.

reject_unknown_database(_Config) ->
  {404, _} = test_lib:req(get, "/dbs/baddatabase"),
  ok.

accept_post(_Config) ->
  DatabaseId = <<"testdabase">>,
  D1 = #{<<"database_id">> => DatabaseId},

  {201, R1} = test_lib:req(post, "/dbs", D1),
  #{<<"database_id">> := DatabaseId} = jsx:decode(R1, [return_maps]),
  ok = barrel_store:delete_db(DatabaseId),

  {201, R2} = test_lib:req(post, "/dbs", #{}),
  #{<<"database_id">> := DatabaseId2} = jsx:decode(R2, [return_maps]),
  ok = barrel_store:delete_db(DatabaseId2),
  ok.

reject_bad_json(_Config) ->
  {400, _} = test_lib:req(post, "/dbs", <<"{badjson">>),
  ok.

dbs(_Config) ->
  {200, R1} = test_lib:req(get, "/dbs"),
  A1 = jsx:decode(R1, [return_maps]),
  [<<"source">>, <<"testdb">>] = A1,
  ok.
