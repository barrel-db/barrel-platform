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

-module(barrel_rest_doc_SUITE).

-export([ all/0
        , end_per_suite/1
        , end_per_testcase/2
        , init_per_suite/1
        , init_per_testcase/2
        ]).

-export([ accept_get/1
        , accept_get_with_rev/1
        , accept_get_with_history/1
        , accept_get_with_id_match/1
        , accept_post/1
        , accept_put/1
        , accept_put_with_etag/1
        , accept_delete/1
        , reject_store_unknown/1
        , reject_unknown_query_parameters/1
        , reject_bad_json/1
        , revsdiff/1
        , put_rev/1
        ]).

all() -> [ accept_get
         , accept_get_with_rev
         , accept_get_with_history
         , accept_get_with_id_match
         , accept_post
         , accept_put
         , accept_put_with_etag
         , accept_delete
         , reject_store_unknown
         , reject_unknown_query_parameters
         , reject_bad_json
         , revsdiff
         , put_rev
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel_store),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel_local:delete_db(<<"testdb">>),
  Config.

end_per_suite(Config) ->
  application:stop(barrel_store),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


accept_get(_Config) ->
  Doc = #{<<"id">> => <<"acceptget">>, <<"name">> => <<"tom">>},
  {ok, _, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  Route = "/dbs/testdb/docs/acceptget",
  {200, R} = test_lib:req(get, Route),
  J = jsx:decode(R, [return_maps]),
  #{<<"name">> := <<"tom">>, <<"_meta">> := #{<<"rev">> := RevId}} = J,
  Url = <<"http://localhost:7080", (list_to_binary(Route))/binary>>,
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  {ok, 200, RespHeaders, _} =  hackney:request(get, Url, Headers, <<>>, []),
  RevId = proplists:get_value(<<"etag">>, RespHeaders),
  ok.

accept_get_with_rev(_Config) ->
  DocId = <<"acceptgetrev">>,
  Doc1 = #{<<"id">> => DocId, <<"v">> => 1},
  {ok, _, RevId1} = barrel_local:post(<<"testdb">>, Doc1, []),
  Doc2 = #{<<"id">> => DocId, <<"v">> => 2},
  {ok, _, RevId2} = barrel_local:put(<<"testdb">>, Doc2, [{rev, RevId1}]),
  {ok, _, _RevId3} = barrel_local:delete(<<"testdb">>, DocId, [{rev, RevId2}]),

  {200, R1} = req_etag(get, "/dbs/testdb/docs/acceptgetrev", #{}, RevId1),
  J1 = jsx:decode(R1, [return_maps]),
  #{<<"v">> := 1, <<"_meta">> := #{<<"rev">> := RevId1}} = J1,
  {200, R2} = req_etag(get, "/dbs/testdb/docs/acceptgetrev", #{}, RevId2),
  J2 = jsx:decode(R2, [return_maps]),
  #{<<"v">> := 2, <<"_meta">> := #{<<"rev">> := RevId2}} = J2,
  ok.

accept_get_with_history(_Config) ->
  Doc = #{<<"id">> => <<"acceptgethist">>, <<"name">> => <<"tom">>},
  {ok, _, _} = barrel_local:post(<<"testdb">>, Doc, []),

  {200, R} = test_lib:req(get, "/dbs/testdb/docs/acceptgethist?history=true"),
  J = jsx:decode(R, [return_maps]),
  #{<<"name">> := <<"tom">>,
    <<"_meta">> := #{<<"revisions">> := Revisions}} = J,
  #{<<"ids">> := [_], <<"start">> := 1} = Revisions,
  ok.

accept_get_with_id_match(_Config) ->
  %% create some docs
  Kvs = [{<<"a">>, 1}, {<<"b">>, 2}, {<<"c">>, 3},
         {<<"d">>, 4}, {<<"e">>, 5}, {<<"f">>, 6},
         {<<"g">>, 42}],
  Docs = [#{ <<"id">> => K, <<"v">> => V} || {K,V} <- Kvs],
  [ {ok,_,_} = barrel_local:post(<<"testdb">>, D, []) || D <- Docs ],

  %% query the HTTP API with x-barrel-id-match header
  Url = <<"http://localhost:7080/dbs/testdb/docs">>,
  Headers = [{<<"Content-Type">>, <<"application/json">>},
             {<<"x-barrel-id-match">>, <<" a , b,c ">>}, %% skipped by cowboy
             {<<"x-barrel-id-match">>, <<"e,f, g">>}], %% only this one is accepted
  {ok, 200, _RespHeaders, Ref} =  hackney:request(get, Url, Headers, <<>>, []),
  {ok, Body} = hackney:body(Ref),
  Json = jsx:decode(Body, [return_maps]),
  #{<<"docs">> := [D1,D2,D3],
    <<"count">> := 3} = Json,
  #{<<"id">> := <<"e">>} = D1,
  #{<<"id">> := <<"f">>} = D2,
  #{<<"id">> := <<"g">>} = D3,
  ok.

accept_post(_Config) ->
  D1 = #{<<"name">> => <<"tom">>},
  {201, R} = test_lib:req(post, "/dbs/testdb/docs", D1),

  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  {ok, Doc, _} = barrel_local:get(<<"testdb">>, DocId, []),
  {409, _} = test_lib:req(post, "/dbs/testdb/docs", Doc),
  ok.

accept_put(_Config) ->
  D1 = #{<<"id">> => <<"a">>, <<"v">> => 1},
  {404, _} = test_lib:req(put, "/dbs/testdb/docs/a", D1),

  {ok, _, _} = barrel_local:post(<<"testdb">>, D1, []),
  D2 = #{<<"id">> => <<"a">>, <<"v">> => 2},
  {201, R} = test_lib:req(put, "/dbs/testdb/docs/a", D2),
  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  {ok, D2, _} = barrel_local:get(<<"testdb">>, DocId, []),
  ok.

accept_put_with_etag(_Config) ->
  D1 = #{<<"id">> => <<"a">>, <<"v">> => 1},
  {404, _} = test_lib:req(put, "/dbs/testdb/docs/a", D1),

  {ok, _, RevId} = barrel_local:post(<<"testdb">>, D1, []),
  D2 = #{<<"id">> => <<"a">>, <<"v">> => 2},
  {201, R} = req_etag(put, "/dbs/testdb/docs/a", D2, RevId),
  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  {ok, D2, _} = barrel_local:get(<<"testdb">>, DocId, []),
  {409, _} = req_etag(put, "/dbs/testdb/docs/a", D2, RevId),
  ok.

req_etag(Method, Route, Doc, Etag) when is_map(Doc) ->
  Headers = [{<<"Content-Type">>, <<"application/json">>},
             {<<"ETag">>, Etag}],
  Body = jsx:encode(Doc),
  Url = <<"http://localhost:7080", (list_to_binary(Route))/binary>>,
  case hackney:request(Method, Url, Headers, Body, []) of
    {ok, Code, _Headers, Ref} ->
      {ok, Answer} = hackney:body(Ref),
      {Code, Answer};
    Error -> Error
  end.

accept_delete(_Config) ->
  {404, _} = test_lib:req(delete, "/dbs/testdb/docs/acceptdelete"),

  %% delete with etag
  Doc = #{<<"id">> => <<"acceptdelete">>, <<"name">> => <<"tom">>},
  {ok, _, RevIdBin} = barrel_local:post(<<"testdb">>, Doc, []),
  RevId = binary_to_list(RevIdBin),
  BadRevId = <<"10-2f25ea96da3fed514795b0ced028d58a">>,
  Url = "/dbs/testdb/docs/acceptdelete",
  {404, _} = req_etag(delete, Url, #{}, BadRevId),
  {200, _} = req_etag(delete, Url, #{}, RevId),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"acceptdelete">>, []),

  %% delete without etag: last winning revision
  Doc2 = #{<<"id">> => <<"deletenoetag">>, <<"name">> => <<"tom">>},
  {ok, _, _} = barrel_local:post(<<"testdb">>, Doc2, []),
  {200, _} = test_lib:req(delete, "/dbs/testdb/docs/deletenoetag", #{}),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"deletenoetag">>, []),
  ok.

reject_store_unknown(_) ->
  Doc = #{<<"name">> => <<"tom">>},
  {400, _} = test_lib:req(get, "/dbs/bad/docs/docid"),
  {400, _} = test_lib:req(put, "/dbs/bad/docs/docid", Doc),
  {400, _} = test_lib:req(post, "/dbs/bad/docs", Doc),
  {400, _} = test_lib:req(delete, "/dbs/bad/docs/docid"),
  ok.

reject_unknown_query_parameters(_) ->
  Doc = #{<<"name">> => <<"tom">>},
  {400, _} = test_lib:req(get, "/dbs/testdb/docs/docid?badparam=whatever"),
  {400, _} = test_lib:req(put, "/dbs/testdb/docs/docid?badparam=whatever", Doc),
  {400, _} = test_lib:req(post, "/dbs/testdb/docs?badparam=whatever", Doc),
  {400, _} = test_lib:req(delete, "/dbs/testdb/docs/docid?badparam=whatever"),
  ok.

reject_bad_json(_) ->
  BadJson = "{\"name\": \"badjson",
  {400, _} = test_lib:req(put, "/dbs/testdb/docs/docid", BadJson),
  NoId = "{\"name\": \"whatever\"}",
  {400, _} = test_lib:req(put, "/dbs/testdb/docs/docid", NoId),
  ok.


post_cat() ->
  Doc = "{\"id\": \"cat\", \"name\" : \"tom\"}",
  {201, R} = test_lib:req(post, "/dbs/testdb/docs/cat", Doc),
  J = jsx:decode(R, [return_maps]),
  binary_to_list(maps:get(<<"rev">>, J)).

revsdiff(_Config) ->
  CatRevId = post_cat(),
  Request = #{<<"cat">> => [CatRevId, <<"2-missing">>]},
  {200, R} = test_lib:req(post, "/dbs/testdb/revsdiff", Request),
  A = jsx:decode(R, [return_maps]),
  CatDiffs = maps:get(<<"cat">>, A),
  Missing = maps:get(<<"missing">>, CatDiffs),
  true = lists:member(<<"2-missing">>, Missing),
  ok.

put_rev(_Config) ->
  RevId = post_cat(),
  {ok, Doc, _Meta} = barrel_local:get(<<"testdb">>, <<"cat">>, []),
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc),
  History = [NewRev, RevId],
  Request = #{<<"id">> => cat,
              <<"document">> => Doc,
              <<"history">> => History},
  {201, R} = test_lib:req(put, "/dbs/testdb/docs/cat?edit=true", Request),
  A = jsx:decode(R, [return_maps]),
  true = maps:get(<<"ok">>, A),
  ok.

