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
        , accept_post/1
        , accept_put/1
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
         , accept_post
         , accept_put
         , accept_delete
         , reject_store_unknown
         , reject_unknown_query_parameters
         , reject_bad_json
         , revsdiff
         , put_rev
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel:delete_db(<<"testdb">>),
  Config.

end_per_suite(Config) ->
  application:stop(barrel),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


accept_get(_Config) ->
  Doc = #{<<"id">> => <<"acceptget">>, <<"name">> => <<"tom">>},
  {ok, _, _} = barrel:put(<<"testdb">>, Doc, []),

  {200, R} = test_lib:req(get, "/testdb/acceptget"),
  J = jsx:decode(R, [return_maps]),
  #{<<"name">> := <<"tom">>} = J,
  ok.

accept_get_with_rev(_Config) ->
  DocId = <<"acceptgetrev">>,
  Doc1 = #{<<"id">> => DocId, <<"v">> => 1},
  {ok, _, RevId1} = barrel:put(<<"testdb">>, Doc1, []),
  Doc2 = #{<<"id">> => DocId, <<"v">> => 2, <<"_rev">> => RevId1},
  {ok, _, RevId2} = barrel:put(<<"testdb">>, Doc2, []),
  {ok, _, _RevId3} = barrel:delete(<<"testdb">>, DocId, RevId2, []),

  {200, R1} = test_lib:req(get, "/testdb/acceptgetrev?rev=" ++ binary_to_list(RevId1)),
  J1 = jsx:decode(R1, [return_maps]),
  #{<<"v">> := 1} = J1,
  {200, R2} = test_lib:req(get, "/testdb/acceptgetrev?rev=" ++ binary_to_list(RevId2)),
  J2 = jsx:decode(R2, [return_maps]),
  #{<<"v">> := 2} = J2,
  ok.

accept_get_with_history(_Config) ->
  Doc = #{<<"id">> => <<"acceptgethist">>, <<"name">> => <<"tom">>},
  {ok, _, _} = barrel:put(<<"testdb">>, Doc, []),

  {200, R} = test_lib:req(get, "/testdb/acceptgethist?history=true"),
  J = jsx:decode(R, [return_maps]),
  #{<<"name">> := <<"tom">>,
    <<"_revisions">> := Revisions} = J,
  #{<<"ids">> := [_], <<"start">> := 1} = Revisions,
  ok.

accept_post(_Config) ->
  D1 = #{<<"name">> => <<"tom">>},
  {201, R} = test_lib:req(post, "/testdb", D1),

  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  {ok, Doc} = barrel:get(<<"testdb">>, DocId, []),
  #{<<"name">> := <<"tom">>} = Doc,
  ok.

accept_put(_Config) ->
  D1 = #{<<"id">> => <<"cat">>, <<"name">> => <<"tom">>},
  {201, R} = test_lib:req(put, "/testdb/cat", D1),

  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  {ok, Doc} = barrel:get(<<"testdb">>, DocId, []),
  #{<<"name">> := <<"tom">>} = Doc,
  ok.

accept_delete(_Config) ->
  Doc = #{<<"id">> => <<"acceptdelete">>, <<"name">> => <<"tom">>},
  {ok, _, RevIdBin} = barrel:put(<<"testdb">>, Doc, []),
  RevId = binary_to_list(RevIdBin),

  Url = "/testdb/acceptdelete?rev=" ++ RevId,
  {200, _} = test_lib:req(delete, Url),

  {error, not_found} = barrel:get(<<"testdb">>, <<"acceptdelete">>, []),
  ok.

reject_store_unknown(_) ->
  Doc = #{<<"name">> => <<"tom">>},
  {400, _} = test_lib:req(get, "/bad/docid"),
  {400, _} = test_lib:req(put, "/bad/docid", Doc),
  {400, _} = test_lib:req(post, "/bad", Doc),
  {400, _} = test_lib:req(delete, "/bad/docid"),
  ok.

reject_unknown_query_parameters(_) ->
  Doc = #{<<"name">> => <<"tom">>},
  {400, _} = test_lib:req(get, "/testdb/docid?badparam=whatever"),
  {400, _} = test_lib:req(put, "/testdb/docid?badparam=whatever", Doc),
  {400, _} = test_lib:req(post, "/testdb?badparam=whatever", Doc),
  {400, _} = test_lib:req(delete, "/testdb/docid?badparam=whatever"),
  ok.

reject_bad_json(_) ->
  BadJson = "{\"name\": \"badjson",
  {400, _} = test_lib:req(put, "/testdb/docid", BadJson),
  NoId = "{\"name\": \"whatever\"}",
  {400, _} = test_lib:req(put, "/testdb/docid", NoId),
  ok.


put_cat() ->
  Doc = "{\"id\": \"cat\", \"name\" : \"tom\"}",
  {201, R} = test_lib:req(put, "/testdb/cat", Doc),
  J = jsx:decode(R, [return_maps]),
  binary_to_list(maps:get(<<"rev">>, J)).

revsdiff(_Config) ->
  CatRevId = put_cat(),
  Request = #{<<"cat">> => [CatRevId, <<"2-missing">>]},
  {200, R} = test_lib:req(post, "/testdb/_revs_diff", Request),
  A = jsx:decode(R, [return_maps]),
  CatDiffs = maps:get(<<"cat">>, A),
  Missing = maps:get(<<"missing">>, CatDiffs),
  true = lists:member(<<"2-missing">>, Missing),
  ok.

put_rev(_Config) ->
  RevId = put_cat(),
  {ok, Doc} = barrel:get(<<"testdb">>, <<"cat">>, []),
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc),
  History = [NewRev, RevId],
  Request = #{<<"id">> => cat,
              <<"document">> => Doc,
              <<"history">> => History},
  {201, R} = test_lib:req(put, "/testdb/cat?edit=true", Request),
  A = jsx:decode(R, [return_maps]),
  true = maps:get(<<"ok">>, A),
  ok.

