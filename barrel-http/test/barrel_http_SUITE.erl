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

-module(barrel_http_SUITE).

-export([all/0,
         end_per_suite/1,
         end_per_testcase/2,
         init_per_suite/1,
         init_per_testcase/2]).

-export([info_database/1,
         post/1,
         put_get_delete/1,
         delete_require_rev_parameter/1,
         two_databases_on_same_store/1,
         revsdiff/1,
         put_rev/1,
         all_docs/1,
         changes_normal/1,
         changes_longpoll/1,
         changes_eventsource/1,
         create_database/1,
         system_doc/1,
         create_replicate_task/1]).

all() -> [ info_database
         , post
         , put_get_delete
         , delete_require_rev_parameter
         , two_databases_on_same_store
         , revsdiff
         , put_rev
         , all_docs
         , changes_normal
         , changes_longpoll
         , changes_eventsource
         , create_database
         , system_doc
         , create_replicate_task
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  Config.

init_per_testcase(_, Config) ->
  ok = barrel_db:start(<<"testdb">>, testdb, [{create_if_missing, true}]),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel_db:clean(<<"testdb">>),
  Config.

end_per_suite(Config) ->
  catch erocksdb:destroy(<<"testdb">>), Config.

%% ----------

info_database(_Config) ->
  {200, R1} = req(get, "/testdb/testdb"),
  A1 = jsx:decode(R1, [return_maps]),
  <<"testdb">> = maps:get(<<"name">>, A1),
  %% TODO refactor
  %% {404, _} = req(get, "/unknwondb"),
  ok.

create_database(_Config) ->
  Cat = "{\"_id\": \"cat\", \"name\" : \"tom\"}",
  {404, _} = req(put, "/testdb/newdb/cat", Cat),
  {201, _} = req(put, "/testdb/newdb", []),
  {200, _} = req(put, "/testdb/newdb/cat", Cat),
  ok.

post(_Config) ->
  D1 = "{\"name\" : \"tom\"}",
  {200, R} = req(post, "/testdb/testdb", D1),
  J = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, J),
  Url = <<"/testdb/testdb/", DocId/binary>>,
  {200, R2} = req(get, Url),
  D2 = jsx:decode(R2, [return_maps]),
  <<"tom">> = maps:get(<<"name">>, D2),
  ok.


put_get_delete(_Config) ->
  CatRevId = put_cat(),

  {200, R2} = req(get, "/testdb/testdb/cat"),
  A2 = jsx:decode(R2, [return_maps]),
  <<"tom">> = maps:get(<<"name">>, A2),

  A3 = delete_cat(CatRevId),

  RevId2 = binary_to_list(maps:get(<<"rev">>, A3)),
  {200, R4} = req(get, "/testdb/testdb/cat?rev=" ++ RevId2),
  A4 = jsx:decode(R4, [return_maps]),
  true = maps:get(<<"_deleted">>, A4),
  ok.

delete_require_rev_parameter(_Config)->
  put_cat(),
  {400, _} = req(delete, "/testdb/testdb/cat?badparametername=42"),
  ok.


put_cat() ->
  Doc = "{\"_id\": \"cat\", \"name\" : \"tom\"}",
  {200, R} = req(put, "/testdb/testdb/cat", Doc),
  J = jsx:decode(R, [return_maps]),
  binary_to_list(maps:get(<<"rev">>, J)).


delete_cat(CatRevId) ->
  {200, R3} = req(delete, "/testdb/testdb/cat?rev=" ++ CatRevId),
  A3 = jsx:decode(R3, [return_maps]),
  true = maps:get(<<"ok">>, A3),
  A3.

put_dog() ->
  Doc = "{\"_id\": \"dog\", \"name\": \"spike\"}",
  {200, R} = req(put, "/testdb/testdb/dog", Doc),
  J = jsx:decode(R, [return_maps]),
  binary_to_list(maps:get(<<"rev">>, J)).

two_databases_on_same_store(_Config) ->
  Cat = "{\"_id\": \"cat\", \"name\" : \"tom\"}",
  Dog = "{\"_id\": \"dog\", \"name\": \"spike\"}",
  {201, _} = req(put, "/testdb/db1", []),
  {201, _} = req(put, "/testdb/db2", []),
  {200, _} = req(put, "/testdb/db1/cat", Cat),
  {200, _} = req(put, "/testdb/db2/dog", Dog),
  {200, _} = req(get, <<"/testdb/db1/cat">>),
  {404, _} = req(get, <<"/testdb/db2/cat">>),
  {200, _} = req(get, <<"/testdb/db2/dog">>),
  {404, _} = req(get, <<"/testdb/db1/dog">>),
  ok.

revsdiff(_Config) ->
  CatRevId = put_cat(),
  Request = #{<<"cat">> => [CatRevId, <<"2-missing">>]},
  {200, R} = req(post, "/testdb/testdb/_revs_diff", Request),
  A = jsx:decode(R, [return_maps]),
  CatDiffs = maps:get(<<"cat">>, A),
  Missing = maps:get(<<"missing">>, CatDiffs),
  true = lists:member(<<"2-missing">>, Missing),
  ok.

put_rev(_Config) ->
  RevId = put_cat(),
  {ok, Doc} = barrel_db:get(<<"testdb">>, <<"cat">>, []),
  {Pos, _} = barrel_doc:parse_revision(RevId),
  NewRev = barrel_doc:revid(Pos +1, RevId, Doc),
  History = [NewRev, RevId],
  Request = #{<<"document">> => Doc,
              <<"history">> => History},
  {200, R} = req(put, "/testdb/testdb/cat/_revs", Request),
  A = jsx:decode(R, [return_maps]),
  true = maps:get(<<"ok">>, A),
  ok.

all_docs(_Config) ->
  {200, R1} = req(get, "/testdb/testdb/_all_docs"),
  A1 = jsx:decode(R1, [return_maps, {labels, attempt_atom}]),
  Rows1 = maps:get(rows, A1),
  0 = length(Rows1),

  put_cat(),
  DogRevId = put_dog(),

  {200, R2} = req(get, "/testdb/testdb/_all_docs"),
  A2 = jsx:decode(R2, [return_maps, {labels, attempt_atom}]),
  Rows2 = maps:get(rows, A2),
  2 = length(Rows2),
  Row = hd(Rows2),
  <<"dog">> = maps:get(id, Row),
  DogRevId = binary_to_list(maps:get(rev, Row)),
  ok.

system_doc(_Config) ->
  Doc = "{\"_id\": \"cat\", \"name\" : \"tom\"}",
  {201, _} = req(put, "/testdb/testdb/_system/cat", Doc),
  {200, R} = req(get, <<"/testdb/db1/cat">>),
  J = jsx:decode(R, [return_maps]),
  #{<<"name">> := <<"tom">>} = J,
  {200, _} = req(put, "/testdb/testdb/_system/cat", "{}"),
  ok.

create_replicate_task(_Config) ->
  %% create 2 databases
  {201, _} = req(put, "/testdb/dba", []),
  {201, _} = req(put, "/testdb/dbb", []),

  {404, _} = req(get, "/testdb/dbb/mouse"),
  %% create a replication task from one db to the other
  Request = #{<<"source">> => <<"http://localhost:8080/testdb/dba">>,
              <<"target">> => <<"http://localhost:8080/testdb/dbb">>},
  {200, R} = req(post, "/_replicate", Request),
  #{<<"repid">> := RepIdBin} = jsx:decode(R, [return_maps]),
  RepId = binary_to_list(RepIdBin),

  %% put one doc in source db
  Mouse = "{\"_id\": \"mouse\", \"name\" : \"jerry\"}",
  {200, _} = req(put, "/testdb/dba/mouse", Mouse),
  timer:sleep(500),
  %% retrieve it replicated in target db
  {200, _} = req(get, "/testdb/dbb/mouse"),

  {404, _} = req(get, "/_replicate/doesnotexist"),
  {200, R2} = req(get, "/_replicate/" ++ RepId),
  Metrics = jsx:decode(R2, [return_maps]),
  #{<<"docs_read">> := 1,
    <<"docs_written">> := 1} = Metrics,
  ok.


%%=======================================================================

changes_normal(_Config) ->
  put_cat(),
  put_dog(),

  {200, R1} = req(get, "/testdb/testdb/_changes"),
  A1 = jsx:decode(R1, [return_maps]),
  2 = maps:get(<<"last_seq">>, A1),
  Results1 = maps:get(<<"results">>, A1),
  2 = length(Results1),

  {200, R2} = req(get, "/testdb/testdb/_changes?since=1"),
  A2 = jsx:decode(R2, [return_maps]),
  2 = maps:get(<<"last_seq">>, A2),
  Results2 = maps:get(<<"results">>, A2),
  1 = length(Results2),
  ok.

%%=======================================================================

changes_longpoll(_Config) ->
  Url = <<"http://localhost:8080/testdb/testdb/_changes?feed=longpoll">>,
  Opts = [async, once],
  {ok, ClientRef} = hackney:get(Url, [], <<>>, Opts),
  CatRevId = put_cat(),
  delete_cat(CatRevId),
  LoopFun = fun(Loop, Ref) ->
                receive
                  {hackney_response, Ref, {status, StatusInt, _Reason}} ->
                    200 = StatusInt,
                    Loop(Loop, Ref);
                  {hackney_response, Ref, {headers, _Headers}} ->
                    Loop(Loop, Ref);
                  {hackney_response, Ref, done} ->
                    ok;
                  {hackney_response, Ref, <<>>} ->
                    Loop(Loop, Ref);
                  {hackney_response, Ref, Bin} ->
                    R = jsx:decode(Bin,[return_maps]),
                    Results = maps:get(<<"results">>, R),
                    [_OnlyOneChange] = Results,
                    Loop(Loop, Ref);

                  _Else ->
                    ok
                after 2000 ->
                    {error, timeout}
                end
            end,
  ok = LoopFun(LoopFun, ClientRef),
  {ok, ClientRef2} = hackney:get(Url, [], <<>>, Opts),
  ok=  LoopFun(LoopFun, ClientRef2).

%%=======================================================================

changes_eventsource(_Config) ->
  Self = self(),
  Pid = spawn(fun () -> wait_response([], 4, Self) end),
  Url = <<"http://localhost:8080/testdb/testdb/_changes?feed=eventsource">>,
  Opts = [async, {stream_to, Pid}],
  {ok, Ref} = hackney:get(Url, [], <<>>, Opts),
  CatRevId = put_cat(),
  delete_cat(CatRevId),
  receive
    {Ref, done} ->
      ct:fail(expected_more_data);
    {Msgs, received_as_expected} ->
      [[200, <<"OK">>], _Headers, _CatPut, _CatDelete] = Msgs
  after 2000 ->
      ct:fail(eventsource_timeout)
  end.

wait_response(Msgs, 0, Parent) ->
  Parent ! {lists:reverse(Msgs), received_as_expected};
wait_response(Msgs, Expected, Parent) ->
  N = Expected - 1,
  receive
    {hackney_response, _Ref, {status, StatusInt, Reason}} ->
      wait_response([[StatusInt,Reason]|Msgs], N, Parent);
    {hackney_response, _Ref, {headers, Headers}} ->
      wait_response([Headers|Msgs], N, Parent);
    {hackney_response, Ref, done} ->
      Parent ! {Ref, Msgs, done},
      ok;
    {hackney_response, _Ref, Bin} ->
      wait_response([Bin|Msgs], N, Parent);
    _Else ->
      ok
  end.

%%=======================================================================

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
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  {ok, Code, _Headers, Ref} = hackney:request(Method, Path, Headers, Body, []),
  {ok, Answer} = hackney:body(Ref),
  {Code, Answer}.
