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

-module(barrel_httpc_SUITE).

-export([all/0,
         end_per_suite/1,
         end_per_testcase/2,
         init_per_suite/1,
         init_per_testcase/2]).

-export([info_database/1,
         create_doc/1,
         put_get_delete/1,
         delete_require_rev_parameter/1,
         revsdiff/1
         %% changes_normal/1,
         %% changes_longpoll/1
        ]).

all() -> [info_database,
          create_doc,
          put_get_delete,
          delete_require_rev_parameter,
          revsdiff
          %% changes_normal,
          %% changes_longpoll
         ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(barrel),
    Config.

init_per_testcase(_, Config) ->
    ok = barrel_db:start(<<"testdb">>, barrel_test_rocksdb),
    Config.

end_per_testcase(_, Config) ->
    ok = barrel_db:clean(<<"testdb">>),
    Config.

end_per_suite(Config) ->
    catch erocksdb:destroy(<<"testdb">>),
    Config.

%% ----------

url() ->
    <<"http://localhost:8080/testdb">>.

info_database(_Config) ->
    {ok, Info} = barrel_httpc:infos(url()),
    <<"testdb">> = maps:get(<<"name">>, Info),
    ok.

create_doc(_Config) ->
    Doc = #{<<"v">> => 1},
    {ok, DocId, RevId} =  barrel_httpc:post(url(), Doc, []),
    CreatedDoc = Doc#{ <<"_id">> => DocId, <<"_rev">> => RevId},
    {ok, CreatedDoc} = barrel_httpc:get(url(), DocId, []),
    {error, not_found} =  barrel_httpc:post(url(), CreatedDoc, []),
    Doc2 = #{<<"_id">> => <<"b">>, <<"v">> => 1},
    {ok, <<"b">>, _RevId2} =  barrel_httpc:post(url(), Doc2, []).


put_get_delete(_Config) ->
    {error, not_found} = barrel_httpc:get(url(), <<"a">>, []),
    Doc = #{ <<"_id">> => <<"a">>, <<"v">> => 1},
    {ok, <<"a">>, RevId} = barrel_httpc:put(url(), <<"a">>, Doc, []),
    Doc2 = Doc#{<<"_rev">> => RevId},
    {ok, Doc2} = barrel_httpc:get(url(), <<"a">>, []),
    {ok, <<"a">>, _RevId2} = barrel_httpc:delete(url(), <<"a">>, RevId, []),
    {ok, DeletedDoc} = barrel_httpc:get(url(), <<"a">>, []),
    true = maps:get(<<"_deleted">>, DeletedDoc).

delete_require_rev_parameter(_Config)->
    put_cat(),
    {400, _} = req(delete, "/testdb/cat?badparametername=42"),
    ok.

put_cat() ->
    Doc = "{\"name\" : \"tom\"}",
    {200, R} = req(put, "/testdb/cat", Doc),
    J = jsx:decode(R, [return_maps]),
    binary_to_list(maps:get(<<"rev">>, J)).


delete_cat(CatRevId) ->
    {200, R3} = req(delete, "/testdb/cat?rev=" ++ CatRevId),
    A3 = jsx:decode(R3, [return_maps]),
    true = maps:get(<<"ok">>, A3),
    A3.

put_dog() ->
    Doc = "{\"name\" : \"spike\"}",
    {200, R} = req(put, "/testdb/dog", Doc),
    J = jsx:decode(R, [return_maps]),
    binary_to_list(maps:get(<<"rev">>, J)).


revsdiff(_Config) ->
    CatRevId = put_cat(),
    Request = #{<<"cat">> => [CatRevId, <<"2-missing">>]},
    {200, R} = req(post, "/testdb/_revs_diff", Request),
    A = jsx:decode(R, [return_maps]),
    CatDiffs = maps:get(<<"cat">>, A),
    Missing = maps:get(<<"missing">>, CatDiffs),
    true = lists:member(<<"2-missing">>, Missing),
    ok.


%% ----------

req(Method, Route) ->
    req(Method,Route,[]).

req(Method, Route, Map) when is_map(Map) ->
    Body = jsx:encode(Map),
    req(Method, Route, Body);

req(Method, Route, String) when is_list(String) ->
    Body = list_to_binary(String),
    req(Method, Route, Body);

req(Method, Route, Body) when is_binary(Body) ->
    Server = "http://localhost:8080",
    Path = list_to_binary(Server ++ Route),
    {ok, Code, _Headers, Ref} = hackney:request(Method, Path, [], Body, []),
    {ok, Answer} = hackney:body(Ref),
    {Code, Answer}.
