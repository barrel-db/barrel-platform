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
  store_exists/1,
  create_db/1
]).

-include("barrel.hrl").

all() ->
  [
    store_exists,
    create_db
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
  Config.

end_per_suite(Config) ->
  application:stop(barrel),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_Config) ->
  ok.

store_exists(_Config) ->
  true = filelib:is_dir("docs"),
  ok.

create_db(_Config) ->
  {ok, #db{name = <<"testdb">>}} = barrel_store:create_db(<<"testdb">>, #{}),
  [<<"testdb">>] = barrel_store:databases(),
  {error, db_exists} = barrel_store:create_db(<<"testdb">>, #{}),
  {ok, #db{name = <<"testdb1">>}} = barrel_store:create_db(<<"testdb1">>, #{}),
  [<<"testdb">>, <<"testdb1">>] = barrel_store:databases(),
  ok = barrel_store:delete_db(<<"testdb">>),
  [<<"testdb1">>] = barrel_store:databases().
  
  

