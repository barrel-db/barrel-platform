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
  order_by_key/1,
  multiple_docs/1
]).

all() ->
  [
   order_by_key%,
    %%multiple_docs
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
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
  {ok, <<"AndersenFamily">>, _Rev} = barrel_local:post(<<"testdb">>, Doc, []),
  timer:sleep(400),
  {ok, _Doc1, _Meta1} = barrel_local:get(<<"testdb">>, <<"AndersenFamily">>, []),

  Fun = fun(D, _Meta, Acc) -> {ok, [maps:get(<<"id">>, D) | Acc]} end,
  [<<"AndersenFamily">>] = barrel_local:walk(<<"testdb">>, <<"id">>, Fun, [], []),
  ok.


multiple_docs(_Config) ->
  DocA = #{ <<"test">> => <<"a">> },
  DocB = #{ <<"test">> => <<"b">> },
  BatchA = [{post, DocA} || _I <- lists:seq(1, 30)],
  BatchB = [{post, DocB} || _I <- lists:seq(1, 25)],
  ResultsA = barrel_local:write_batch(<<"testdb">>, BatchA, []),
  ResultsB = barrel_local:write_batch(<<"testdb">>, BatchB, []),

  IdsA = [Id || {ok, Id, _} <- ResultsA],
  IdsB = [Id || {ok, Id, _} <- ResultsB],

  30 = length(IdsA),
  25 = length(IdsB),

  All = barrel_local:fold_by_id(<<"testdb">>,
                                       fun(#{ <<"id">> := Id }, _, Acc) -> {ok, [Id | Acc]} end,
                                       [],
                                       []),
  55 = length(All),

  All20 = barrel_local:fold_by_id(<<"testdb">>,
                                       fun(#{ <<"id">> := Id }, _, Acc) -> {ok, [Id | Acc]} end,
                                       [],
                                       [{max, 20}]),
  20 = length(All20),

  All40 = barrel_local:fold_by_id(<<"testdb">>,
                                       fun(#{ <<"id">> := Id }, _, Acc) -> {ok, [Id | Acc]} end,
                                       [],
                                       [{max, 40}]),
  40 = length(All40),

  QAll = barrel_local:query(<<"testdb">>,
                                  <<"test/a">>,
                                  fun(Id, _, _, Acc) -> {ok, [Id | Acc]} end,
                                  [],
                                  []),
  30 = length(QAll),

  Q15 = barrel_local:query(<<"testdb">>,
                                  <<"test/a">>,
                                  fun(Id, _, _, Acc) -> {ok, [Id | Acc]} end,
                                  [],
                                  [{max, 15}]),
  15 = length(Q15),

  QBAll = barrel_local:query(<<"testdb">>,
                                  <<"test/b">>,
                                  fun(Id, _, _, Acc) -> {ok, [Id | Acc]} end,
                                  [],
                                  []),
  25 = length(QBAll).

