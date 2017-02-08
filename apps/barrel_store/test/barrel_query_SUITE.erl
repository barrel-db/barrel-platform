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
-module(barrel_query_SUITE).
-author("benoitc").


-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([
  order_by_key/1
]).

all() ->
  [
    order_by_key
  ].

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


order_by_key(_Config) ->
  Doc = #{
    <<"id">> => <<"AndersenFamily">>,
    <<"lastName">> => <<"Andersen">>,
    <<"parents">> => [
      #{ <<"firstName">> => <<"Thomas">> },
      #{ <<"firstName">> => <<"Mary Kay">>}
    ],
    <<"children">> => [
      #{
        <<"firstName">> => <<"Henriette Thaulow">>, <<"gender">> => <<"female">>, <<"grade">> =>  5,
        <<"pets">> => [#{ <<"givenName">> => <<"Fluffy">> }]
      }
    ],
    <<"address">> => #{ <<"state">> => <<"WA">>, <<"county">> => <<"King">>, <<"city">> => <<"seattle">> },
    <<"creationDate">> => 1431620472,
    <<"isRegistered">> => true
  },
  {ok, <<"AndersenFamily">>, _Rev} = barrel_local:put(<<"testdb">>, Doc, []),
  timer:sleep(400),
  {ok, _Doc1} = barrel_local:get(<<"testdb">>, <<"AndersenFamily">>, []),

  Fun = fun(Id, _D, V, Acc) -> {ok, [{Id, V} | Acc]} end,
  [{<<"AndersenFamily">>, <<"AndersenFamily">>}] = barrel_local:query(<<"testdb">>, <<"id">>, Fun, [], []),
  [{<<"AndersenFamily">>, null}] = barrel_local:find_by_key(<<"testdb">>, <<"id/AndersenFamily">>, Fun, [], [] ),
  ok.

