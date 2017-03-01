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

r(Req) ->
  test_lib:req(Req).

accept_get(_Config) ->
  Doc = #{<<"id">> => <<"acceptget">>, <<"name">> => <<"tom">>},
  {ok, _, RevId} = barrel_local:post(<<"testdb">>, Doc, []),

  #{code := 200,
    doc := Doc,
    headers := H} = r(#{method => get,
                        route => "/dbs/testdb/docs/acceptget"}),

  RevId = proplists:get_value(<<"ETag">>, H),
  ok.

accept_get_with_rev(_Config) ->
  DocId = <<"acceptgetrev">>,
  Doc1 = #{<<"id">> => DocId, <<"v">> => 1},
  {ok, _, RevId1} = barrel_local:post(<<"testdb">>, Doc1, []),
  Doc2 = #{<<"id">> => DocId, <<"v">> => 2},
  {ok, _, RevId2} = barrel_local:put(<<"testdb">>, Doc2, [{rev, RevId1}]),
  {ok, _, RevId3} = barrel_local:delete(<<"testdb">>, DocId, [{rev, RevId2}]),
  Route = "/dbs/testdb/docs/acceptgetrev",

  #{code := 200,
    doc := Doc1,
    headers := H1} = r(#{method => get,
                         headers => [{"etag", RevId1}],
                         route => Route}),
  RevId1 = proplists:get_value(<<"ETag">>, H1),

  #{code := 200,
    doc := Doc2,
    headers := H2} = r(#{method => get,
                         headers => [{"etag", RevId2}],
                         route => Route}),
  RevId2 = proplists:get_value(<<"ETag">>, H2),

  #{code := 200,
    headers := H3} = r(#{method => get,
                         headers => [{"etag", RevId3}],
                         route => Route}),
  <<"true">> = proplists:get_value(<<"x-barrel-deleted">>, H3),
  ok.

accept_get_with_history(_Config) ->
  D1 = #{<<"id">> => <<"acceptgethist">>, <<"v">> => 1},
  {ok, _, R1} = barrel_local:post(<<"testdb">>, D1, []),
  D2 = D1#{<<"v">> => 2},
  {ok, _, R2} = barrel_local:put(<<"testdb">>, D2, []),

  #{code := 200,
    headers := Headers,
    doc := D2} = r(#{method => get,
                     route => "/dbs/testdb/docs/acceptgethist?history=true"}),

  RevIds = proplists:get_value(<<"x-barrel-revisions-id">>, Headers),
  [R2, R1] = binary:split(RevIds, <<",">>),
  ok.

accept_get_with_id_match(_Config) ->
  %% create some docs
  Kvs = [{<<"a">>, 1}, {<<"b">>, 2}, {<<"c">>, 3},
         {<<"d">>, 4}, {<<"e">>, 5}, {<<"f">>, 6},
         {<<"g">>, 42}],
  Docs = [#{ <<"id">> => K, <<"v">> => V} || {K,V} <- Kvs],
  [ {ok,_,_} = barrel_local:post(<<"testdb">>, D, []) || D <- Docs ],

  %% query the HTTP API with x-barrel-id-match header
  #{code := 200,
    doc := J} = r(#{method => get,
                    headers => [{<<"x-barrel-id-match">>, <<"a, b, c">>},
                                {<<"x-barrel-id-match">>, <<"e,f, g">>}],
                    route => "/dbs/testdb/docs"}),

  #{<<"docs">> := [D1,D2,D3,D4|_],
    <<"count">> := 6} = J,
  #{<<"id">> := <<"a">>} = D1,
  #{<<"id">> := <<"b">>} = D2,
  #{<<"id">> := <<"c">>} = D3,
  #{<<"id">> := <<"e">>} = D4,
  ok.

accept_post(_Config) ->
  D1 = #{<<"name">> => <<"tom">>},
  #{code := 201,
    headers := H,
    doc := J} = r(#{method => post,
                    body => D1,
                    route => "/dbs/testdb/docs"}),

  RevId = proplists:get_value(<<"etag">>, H),
  DocId = maps:get(<<"id">>, J),
  {ok, Doc, _} = barrel_local:get(<<"testdb">>, DocId, [{rev, RevId}]),
  {409, _} = test_lib:req(post, "/dbs/testdb/docs", Doc),
  ok.

accept_put(_Config) ->
  D1 = #{<<"id">> => <<"a">>, <<"v">> => 1},
  #{code := 404} = r(#{method => put,
                       body => D1,
                       route => "/dbs/testdb/docs/a"}),

  {ok, _, _} = barrel_local:post(<<"testdb">>, D1, []),
  D2 = #{<<"id">> => <<"a">>, <<"v">> => 2},
  #{code := 201,
    headers := H,
    doc := D2} = r(#{method => put,
                    body => D2,
                    route => "/dbs/testdb/docs/a"}),
  RevId = proplists:get_value(<<"etag">>, H),
  {ok, D2, _} = barrel_local:get(<<"testdb">>, <<"a">>, [{rev, RevId}]),
  ok.

accept_put_with_etag(_Config) ->
  D1 = #{<<"id">> => <<"a">>, <<"v">> => 1},
  {404, _} = test_lib:req(put, "/dbs/testdb/docs/a", D1),

  {ok, _, RevId} = barrel_local:post(<<"testdb">>, D1, []),
  D2 = #{<<"id">> => <<"a">>, <<"v">> => 2},

  #{code := 201,
    headers := H,
    doc := D2} = r(#{method => put,
                     body => D2,
                     headers => [{"etag", RevId}],
                     route => "/dbs/testdb/docs/a"}),

  RevId2 = proplists:get_value(<<"etag">>, H),
  {ok, D2, _} = barrel_local:get(<<"testdb">>, <<"a">>, [{rev, RevId2}]),

  #{code := 409} = r(#{method => put,
                       body => D2,
                       headers => [{"etag", RevId}],
                       route => "/dbs/testdb/docs/a"}),
  ok.

accept_delete(_Config) ->
  {404, _} = test_lib:req(delete, "/dbs/testdb/docs/acceptdelete"),

  %% delete with etag
  Doc = #{<<"id">> => <<"acceptdelete">>, <<"name">> => <<"tom">>},
  {ok, _, RevIdBin} = barrel_local:post(<<"testdb">>, Doc, []),
  RevId = binary_to_list(RevIdBin),
  BadRevId = <<"10-2f25ea96da3fed514795b0ced028d58a">>,
  Url = "/dbs/testdb/docs/acceptdelete",

  #{code := 404} = r(#{method => delete,
                       headers => [{"etag", BadRevId}],
                       route => Url}),

  #{code := 200} = r(#{method => delete,
                       headers => [{"etag", RevId}],
                       route => Url}),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"acceptdelete">>, []),

  %% delete without etag: last winning revision
  Doc2 = #{<<"id">> => <<"deletenoetag">>, <<"name">> => <<"tom">>},
  {ok, _, _} = barrel_local:post(<<"testdb">>, Doc2, []),
  #{code := 200} = r(#{method => delete,
                       route => "/dbs/testdb/docs/deletenoetag"}),
  {error, not_found} = barrel_local:get(<<"testdb">>, <<"deletenoetag">>, []),

  %% recreate the same doc
  {ok, _, RevIdBin2} = barrel_local:post(<<"testdb">>, Doc, []),

  %% delete with a correct previous revid (but not the last one.)
  #{code := 409} = r(#{method => delete,
                       headers => [{"etag", RevId}],
                       route => Url}),

  %% delete with the correct last revision
  #{code := 200} = r(#{method => delete,
                       headers => [{"etag", RevIdBin2}],
                       route => Url}),
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
  Doc = #{<<"id">> => <<"cat">>, <<"name">> => <<"tom">>},
  {ok, _, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  RevId.

revsdiff(_Config) ->
  CatRevId = post_cat(),

  #{code := 200,
    doc := A} = r(#{method => post,
                    body => #{<<"cat">> => [CatRevId, <<"2-missing">>]},
                    route => "/dbs/testdb/revsdiff"}),

  CatDiffs = maps:get(<<"cat">>, A),
  Missing = maps:get(<<"missing">>, CatDiffs),
  true = lists:member(<<"2-missing">>, Missing),
  ok.

put_rev(_Config) ->
  RevId = post_cat(),
  {ok, Doc, _Meta} = barrel_local:get(<<"testdb">>, <<"cat">>, []),
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, barrel_doc:make_doc(Doc, <<>>, false)),
  History = [NewRev, RevId],
  Request = #{<<"id">> => cat,
              <<"document">> => Doc,
              <<"history">> => History},
  #{code := 201} = r(#{method => put,
                       route => "/dbs/testdb/docs/cat?edit=true",
                       body => Request}),
  ok.

