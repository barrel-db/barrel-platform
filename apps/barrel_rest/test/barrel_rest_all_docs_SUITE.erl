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

-module(barrel_rest_all_docs_SUITE).

-export([all/0,
         end_per_suite/1,
         end_per_testcase/2,
         init_per_suite/1,
         init_per_testcase/2]).

-export([ accept_get/1
        , accept_start_key/1
        , accept_end_key/1
        , accept_max/1
        , reject_store_unknown/1
        ]).

all() -> [ accept_get
         , accept_start_key
         , accept_end_key
         , accept_max
         , reject_store_unknown
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_rest),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel:create_db(<<"testdb">>, #{}),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel:delete_db(<<"testdb">>),
  Config.


end_per_suite(Config) ->
  _ = application:stop(barrel_rest),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


accept_get(_Config) ->
  {200, R1} = test_lib:req(get, "/dbs/testdb/docs"),
  A1 = jsx:decode(R1, [return_maps, {labels, attempt_atom}]),
  Rows1 = maps:get(docs, A1),
  0 = length(Rows1),

  D1 = #{<<"id">> => <<"cat">>, <<"name">> => <<"tom">>},
  {ok, _, CatRevId} = barrel:post(<<"testdb">>, D1, []),
  D2 = #{<<"id">> => <<"dog">>, <<"name">> => <<"dingo">>},
  {ok, _, DogRevId} = barrel:post(<<"testdb">>, D2, []),

  {200, R2} = test_lib:req(get, "/dbs/testdb/docs"),
  A2 = jsx:decode(R2, [return_maps]),
  Rows2 = maps:get(<<"docs">>, A2),
  [#{<<"meta">> := #{<<"rev">> := CatRevId}, <<"doc">> := D1},
   #{<<"meta">> := #{<<"rev">> := DogRevId}, <<"doc">> := D2}] = Rows2,
  ok.

accept_start_key(_Config) ->
  ok = create_docs(<<"startkey">>, 10, <<"testdb">>),

  {200, R} = test_lib:req(get, "/dbs/testdb/docs?start_key=startkey0004"),
  A = jsx:decode(R, [return_maps]),
  #{<<"docs">> := Rows} = A,
  7 = length(Rows),
  ok.

accept_end_key(_Config) ->
  ok = create_docs(<<"endkey">>, 10, <<"testdb">>),

  {200, R} = test_lib:req(get, "/dbs/testdb/docs?end_key=endkey0004"),
  A = jsx:decode(R, [return_maps]),
  #{<<"docs">> := Rows} = A,
  4 = length(Rows),
  ok.

accept_max(_Config) ->
  ok.

create_docs(_Prefix, 0, _Conn) ->
  ok;
create_docs(Prefix, N, Conn) ->
  S = lists:flatten(io_lib:format("~4..0B",[N])),
  B = list_to_binary(S),
  Key = <<Prefix/binary, B/binary>>,
  Doc = #{<<"id">> => Key, <<"v">> => 1},
  {ok, _, _} = barrel:post(Conn, Doc, []),
  create_docs(Prefix, N-1, Conn).


reject_store_unknown(_Config) ->
  {400, _} = test_lib:req(get, "/dbs/doesnotexist/docs"),
  ok.
