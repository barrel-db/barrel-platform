%% Copyright (c) 2017. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_store_SUITE).
-author("benoitc").

%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/1
]).

-export([
  create_db/1,
  persist_db/1
]).

-include("barrel_store.hrl").

all() ->
  [
    create_db,
    persist_db
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_store),
  Config.

end_per_suite(Config) ->
  application:stop(barrel_store),
  Config.


init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_Config) ->
  ok.

create_db(_Config) ->
  {ok, #{ <<"database_id">> := <<"testdb">>}} = barrel_store:create_db(<<"testdb">>, #{}),
  [<<"testdb">>] = barrel_store:databases(),
  true = barrel_db:exists(<<"testdb">>),
  {error, db_exists} = barrel_store:create_db(<<"testdb">>, #{}),
  {ok, #{ <<"database_id">> := <<"testdb1">>}} = barrel_store:create_db(<<"testdb1">>, #{}),
  [<<"testdb">>, <<"testdb1">>] = barrel_store:databases(),
  ok = barrel_store:delete_db(<<"testdb">>),
  [<<"testdb1">>] = barrel_store:databases(),
  ok = barrel_store:delete_db(<<"testdb1">>),
  timer:sleep(100),
  [] = barrel_store:databases().


persist_db(_Config) ->
  {ok, #{ <<"database_id">> := <<"testdb">>}} = barrel_store:create_db(<<"testdb">>, #{}),
  [<<"testdb">>] = barrel_store:databases(),
  ok = application:stop(barrel_store),
  io:format("stopped the database", []),
  {ok, _} = application:ensure_all_started(barrel_store),
  [<<"testdb">>] = barrel_store:databases(),
  io:format("started the database", []),
  ok = barrel_store:delete_db(<<"testdb">>),
  [] = barrel_store:databases(),
  ok = application:stop(barrel_store),
  timer:sleep(100),
  {ok, _} = application:ensure_all_started(barrel_store),
  [] = barrel_store:databases().
