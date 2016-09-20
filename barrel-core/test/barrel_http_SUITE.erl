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
         put_get_delete/1,
         delete_require_rev_parameter/1,
         revsdiff/1,
         changes_normal/1,
         changes_longpoll/1,
         changes_eventsource/1]).

all() -> [info_database,
          put_get_delete,
          delete_require_rev_parameter,
          revsdiff,
          changes_normal,
          changes_longpoll,
          changes_eventsource].

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
  catch erocksdb:destroy(<<"testdb">>), Config.

%% ----------

info_database(_Config) ->
  {200, R1} = req(get, "/testdb"),
  A1 = jsx:decode(R1, [return_maps]),
  <<"testdb">> = maps:get(<<"name">>, A1),

  {404, _} = req(get, "/unknwondb"),
  ok.

put_get_delete(_Config) ->
  CatRevId = put_cat(),

  {200, R2} = req(get, "/testdb/cat"),
  A2 = jsx:decode(R2, [return_maps]),
  <<"tom">> = maps:get(<<"name">>, A2),

  A3 = delete_cat(CatRevId),

  RevId2 = binary_to_list(maps:get(<<"rev">>, A3)),
  {200, R4} = req(get, "/testdb/cat?rev=" ++ RevId2),
  A4 = jsx:decode(R4, [return_maps]),
  true = maps:get(<<"_deleted">>, A4),
  ok.

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


%%=======================================================================

changes_normal(_Config) ->
  put_cat(),
  put_dog(),

  {200, R1} = req(get, "/testdb/_changes"),
  A1 = jsx:decode(R1, [return_maps]),
  2 = maps:get(<<"last_seq">>, A1),
  Results1 = maps:get(<<"results">>, A1),
  2 = length(Results1),

  {200, R2} = req(get, "/testdb/_changes?since=1"),
  A2 = jsx:decode(R2, [return_maps]),
  2 = maps:get(<<"last_seq">>, A2),
  Results2 = maps:get(<<"results">>, A2),
  1 = length(Results2),
  ok.

%%=======================================================================

changes_longpoll(_Config) ->
  Url = <<"http://localhost:8080/testdb/_changes?feed=longpoll">>,
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
  Url = <<"http://localhost:8080/testdb/_changes?feed=eventsource">>,
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
  {ok, Code, _Headers, Ref} = hackney:request(Method, Path, [], Body, []),
  {ok, Answer} = hackney:body(Ref),
  {Code, Answer}.
