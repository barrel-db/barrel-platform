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


-module(barrel_system_docs_SUITE).
-author("Bernard Notarianni").

%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([write_and_get/1]).

all() -> [write_and_get].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_store),
  Config.

init_per_testcase(_, Config) ->
  {ok, _} = barrel_store:create_db(<<"testdb">>, #{}),
  [{db, <<"testdb">>} | Config].

end_per_testcase(_, _Config) ->
  ok = barrel_store:delete_db(<<"testdb">>),
  ok.

end_per_suite(Config) ->
  application:stop(barrel_store),
  Config.

write_and_get(_Config) ->
  Doc = #{<<"id">> => <<"a">>, <<"v">> => 1},
  ok = barrel_db:put_system_doc(<<"testdb">>, Doc),
  {ok, Doc} = barrel_db:get_system_doc(<<"testdb">>, <<"a">>),
  ok = barrel_db:delete_system_doc(<<"testdb">>, <<"a">>),
  {error, not_found} = barrel_db:get_system_doc(<<"testdb">>, <<"a">>),
  ok.

