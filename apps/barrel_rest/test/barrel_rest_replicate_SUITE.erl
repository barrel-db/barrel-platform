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
        , accept_local_db/1
        , accept_put_get/1
        , accept_delete/1
        , reject_replication_name_unknown/1
        , reject_store_unknown/1
        , reject_bad_json /1
        , list_replication_tasks/1
        ]).

%%all() -> [ accept_post_get
%%         , list_replication_tasks
%%         , accept_local_db
%%         , accept_put_get
%%         , accept_delete
%%         , reject_replication_name_unknown
%%         , reject_store_unknown
%%         , reject_bad_json
%%         ].

all() -> [].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_rest),
  Config.

init_per_testcase(_, Config) ->
  _ = (catch file:delete("data/replication.config")),
  
  {ok, _} = barrel:create_database(#{ <<"database_id">> => <<"dba">> }),
  {ok, _} = barrel:create_database(#{ <<"database_id">> => <<"dbb">> }),
  {ok, _} = barrel:create_database(#{ <<"database_id">> => <<"dbaa">> }),
  {ok, _} = barrel:create_database(#{ <<"database_id">> => <<"dbbb">> }),
  {ok, _} = barrel:create_database(#{ <<"database_id">> => <<"dbaaa">> }),
  {ok, _} = barrel:create_database(#{ <<"database_id">> => <<"dbbbb">> }) ,
  {ok, _} = barrel:create_database(#{ <<"database_id">> => <<"testdb">> }),
  timer:sleep(100),
  Config.

end_per_testcase(_, Config) ->
  _ = barrel:delete_database(<<"dba">>),
  _ = barrel:delete_database(<<"dbb">>),
  _ = barrel:delete_database(<<"dbaa">>),
  _ = barrel:delete_database(<<"dbbb">>),
  _ = barrel:delete_database(<<"dbaaa">>),
  _ = barrel:delete_database(<<"dbbbb">>),
  _ = barrel:delete_database(<<"testdb">>),
  _ = (catch file:delete("data/replication.config")),
  timer:sleep(100),
  Config.

end_per_suite(Config) ->
  ok = application:stop(barrel_rest),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


accept_post_get(_Config) ->
  {404, _} = test_lib:req(get, "/dbs/dbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"dba">>,
              <<"target">> => <<"dbb">>},
  io:format("start accept_post_get test ~n", []),
  {200, R} = test_lib:req(post, "/replicate", Request),
  #{<<"replication_id">> := RepIdBin} = jsx:decode(R, [return_maps]),
  RepId = binary_to_list(RepIdBin),

  %% put one doc in source db
  Mouse = "{\"id\": \"mouse\", \"name\" : \"jerry\"}",
  {201, _} = test_lib:req(post, "/dbs/dba/docs", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = test_lib:req(get, "/dbs/dbb/docs/mouse"),

  {404, _} = test_lib:req(get, "/replicate/doesnotexist"),
  {200, R2} = test_lib:req(get, "/replicate/" ++ RepId),
  Metrics = jsx:decode(R2, [return_maps]),
  #{<<"docs_read">> := 1,  <<"docs_written">> := 1} = Metrics,
  ok = barrel:delete_replication(RepIdBin),
  ok.

list_replication_tasks(_Config) ->
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"dba">>,
              <<"target">> => <<"dbb">>},
  {200, R} = test_lib:req(post, "/replicate", Request),
  #{<<"replication_id">> := RepId} = jsx:decode(R, [return_maps]),
  {200, R2} = test_lib:req(get, "/replicate"),
  #{ <<"tasks">> := Tasks} = jsx:decode(R2, [return_maps]),
  [RepId] = Tasks,
  ok = barrel:delete_replication(RepId),
  ok.

accept_local_db(_Config) ->
  {404, _} = test_lib:req(get, "/dbs/dbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"dba">>, <<"target">> => <<"dbb">>},
  {200, R} = test_lib:req(post, "/replicate", Request),
  #{<<"replication_id">> := RepIdBin} = jsx:decode(R, [return_maps]),
  RepId = binary_to_list(RepIdBin),

  %% put one doc in source db
  Mouse = "{\"id\": \"mouse\", \"name\" : \"jerry\"}",
  {201, _} = test_lib:req(post, "/dbs/dba/docs", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = test_lib:req(get, "/dbs/dbb/docs/mouse"),
  
  {404, _} = test_lib:req(get, "/replicate/doesnotexist"),
  {200, R2} = test_lib:req(get, "/replicate/" ++ RepId),
  Metrics = jsx:decode(R2, [return_maps]),
  #{<<"docs_read">> := 1,
    <<"docs_written">> := 1} = Metrics,
  io:format("replication name ~p~n", [RepId]),
  ok = barrel:delete_replication(RepId),
  ok.

accept_put_get(_Config) ->
  {404, _} = test_lib:req(get, "/dbs/dbbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"dbaa">>,
              <<"target">> => <<"dbbb">>,
              <<"persisted">> => true},
  {200, R} = test_lib:req(put, "/replicate/myreplication", Request),
  #{<<"replication_id">> := <<"myreplication">>} = jsx:decode(R, [return_maps]),

  %% put one doc in source db
  Mouse = "{\"id\": \"mouse\", \"name\" : \"jerry\"}",
  {201, _} = test_lib:req(post, "/dbs/dbaa/docs", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = test_lib:req(get, "/dbs/dbbb/docs/mouse"),

  {200, R2} = test_lib:req(get, "/replicate/myreplication"),
  Metrics = jsx:decode(R2, [return_maps]),
  #{<<"docs_read">> := 1,
    <<"docs_written">> := 1} = Metrics,
  barrel:delete_replication(<<"myreplication">>),
  timer:sleep(100),
  ok.

accept_delete(_Config) ->
  {404, _} = test_lib:req(get, "/dbs/dbbbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"dbaaa">>,
              <<"target">> => <<"dbbbb">>,
              <<"persisted">> => true},
  {200, R} = test_lib:req(put, "/replicate/tasktobedeleted", Request),
  #{<<"replication_id">> := <<"tasktobedeleted">>} = jsx:decode(R, [return_maps]),

  %% put one doc in source db
  Mouse = "{\"id\": \"mouse\", \"name\" : \"jerry\"}",
  {201, _} = test_lib:req(post, "/dbs/dbaaa/docs/mouse", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = test_lib:req(get, "/dbs/dbbbb/docs/mouse"),

  %% delete the replication task
  {200, _} = test_lib:req(get, "/replicate/tasktobedeleted"),
  {200, _} = test_lib:req(delete, "/replicate/tasktobedeleted"),
  {404, _} = test_lib:req(get, "/replicate/tasktobedeleted"),

  %% put another doc in source db
  Cat = "{\"id\": \"cat\", \"name\" : \"tom\"}",
  {201, _} = test_lib:req(post, "/dbs/dbaaa/docs/cat", Cat),
  timer:sleep(500),

  %% it has not been replicated
  {404, _} = test_lib:req(get, "/dbs/dbbbb/docs/cat"),
  ok.


reject_replication_name_unknown(_Config) ->
  {404, _} = test_lib:req(get, "/replicate/unknown"),
  {404, _} = test_lib:req(delete, "/replicate/unknown"),
  ok.

reject_store_unknown(_Config) ->
  M = #{<<"persisted">> => true},
  NoStoreSource = M#{<<"source">> => <<"nostore">>,
                    <<"target">> => <<"dbb">>},
  NoStoreTarget = M#{<<"source">> => <<"dba">>,
                    <<"target">> => <<"nostore">>},

  {400, _} = test_lib:req(post, "/replicate", NoStoreSource),
  {400, _} = test_lib:req(post, "/replicate", NoStoreTarget),
  ok.

reject_bad_json(_Config) ->
  BadJson = "{\"source\": \"badjson no complet",
  NoSource = #{<<"target">> => <<"nostore/dbb">>},
  NoTarget = #{<<"source">> => <<"nostore/dba">>},

  {400, _} = test_lib:req(post, "/replicate", BadJson),
  {400, _} = test_lib:req(post, "/replicate", NoSource),
  {400, _} = test_lib:req(post, "/replicate", NoTarget),
  ok.
