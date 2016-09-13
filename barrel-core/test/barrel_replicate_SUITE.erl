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

-module(barrel_replicate_SUITE).
-author("Bernard Notarianni").

%% API
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         replicate/1,
         replicate_non_empty/1,
         replicate_delete/1
        ]).

all() ->
    [
     replicate,
     replicate_non_empty,
     replicate_delete
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(barrel),
    Config.

init_per_testcase(_, Config) ->
    ok = barrel_db:start(<<"testdb">>, barrel_test_rocksdb),
    ok = barrel_db:start(<<"source">>, barrel_test_rocksdb),
    Config.

end_per_testcase(_, _Config) ->
    ok = barrel_db:clean(<<"testdb">>),
    ok = barrel_db:clean(<<"source">>),
    ok.

end_per_suite(Config) ->
    %ok = erocksdb:destroy("testdb", []),
    %ok = erocksdb:destroy("source", []),
    Config.

replicate(_Config) ->
    {ok, _Pid} = barrel_replicate:start_link(<<"source">>, <<"testdb">>),
    Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
    {ok, <<"a">>, RevId} = barrel_db:put(<<"source">>, <<"a">>, Doc, []),
    Doc2 = Doc#{<<"_rev">> => RevId},
    timer:sleep(200),
    {ok, Doc2} = barrel_db:get(<<"testdb">>, <<"a">>, []),
    stopped = barrel_replicate:stop(),
    ok.

replicate_non_empty(_Config) ->
    Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
    {ok, <<"a">>, RevId} = barrel_db:put(<<"source">>, <<"a">>, Doc, []),
    Doc2 = Doc#{<<"_rev">> => RevId},

    {ok, _Pid} = barrel_replicate:start_link(<<"source">>, <<"testdb">>),
    timer:sleep(200),

    {ok, Doc2} = barrel_db:get(<<"testdb">>, <<"a">>, []),
    stopped = barrel_replicate:stop(),
    ok.

replicate_delete(_Config) ->
    Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
    {ok, <<"a">>, RevId} = barrel_db:put(<<"source">>, <<"a">>, Doc, []),

    {ok, _Pid} = barrel_replicate:start_link(<<"source">>, <<"testdb">>),
    barrel_db:delete(<<"source">>, <<"a">>, RevId, []),
    timer:sleep(200),
    {ok, Doc3} = barrel_db:get(<<"testdb">>, <<"a">>, []),
    true = maps:get(<<"_deleted">>, Doc3),
    stopped = barrel_replicate:stop(),
    ok.
