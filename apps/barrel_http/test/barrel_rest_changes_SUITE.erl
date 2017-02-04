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

-module(barrel_rest_changes_SUITE).

-export([all/0,
         end_per_suite/1,
         end_per_testcase/2,
         init_per_suite/1,
         init_per_testcase/2]).

-export([ accept_get_normal/1
        , accept_get_history_all/1
        , accept_get_longpoll_heartbeat/1
        , accept_get_eventsource/1
        , accept_get_eventsource_headers/1
        , reject_store_unknown/1
        , reject_bad_params/1
        ]).

all() -> [ accept_get_normal
         , accept_get_history_all
         , accept_get_longpoll_heartbeat
         , accept_get_eventsource
         , accept_get_eventsource_headers
         , reject_store_unknown
         , reject_bad_params
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel:delete_db(<<"testdb">>),
  Config.

end_per_suite(Config) ->
  application:stop(barrel_http),
  application:stop(barrel),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.


put_cat() ->
  Doc = "{\"id\": \"cat\", \"name\" : \"tom\"}",
  {201, R} = test_lib:req(put, "/dbs/testdb/docs/cat", Doc),
  J = jsx:decode(R, [return_maps]),
  binary_to_list(maps:get(<<"rev">>, J)).


delete_cat(CatRevId) ->
  {200, R3} = test_lib:req(delete, "/dbs/testdb/docs/cat?rev=" ++ CatRevId),
  A3 = jsx:decode(R3, [return_maps]),
  true = maps:get(<<"ok">>, A3),
  binary_to_list(maps:get(<<"rev">>, A3)).

put_dog() ->
  Doc = "{\"id\": \"dog\", \"name\": \"spike\"}",
  {201, R} = test_lib:req(put, "/dbs/testdb/docs/dog", Doc),
  J = jsx:decode(R, [return_maps]),
  binary_to_list(maps:get(<<"rev">>, J)).


%%=======================================================================

accept_get_normal(_Config) ->
  put_cat(),
  put_dog(),

  {200, R1} = req_changes("/dbs/testdb/docs"),
  A1 = jsx:decode(R1, [return_maps]),
  2 = maps:get(<<"last_seq">>, A1),
  Results1 = maps:get(<<"results">>, A1),
  2 = length(Results1),

  {200, R2} = req_changes("/dbs/testdb/docs?since=1"),
  A2 = jsx:decode(R2, [return_maps]),
  2 = maps:get(<<"last_seq">>, A2),
  Results2 = maps:get(<<"results">>, A2),
  1 = length(Results2),
  ok.

accept_get_history_all(_Config) ->
  CreateRevId = put_cat(),
  put_dog(),
  DeleteRevId = delete_cat(CreateRevId),

  {200, R1} = req_changes("/dbs/testdb/docs?history=all"),
  A1 = jsx:decode(R1, [return_maps]),
  3 = maps:get(<<"last_seq">>, A1),
  Results1 = maps:get(<<"results">>, A1),
  2 = length(Results1),
  #{<<"id">> := <<"cat">>,
    <<"changes">> := CatHistory} = hd(Results1),
  [DeleteRevId, CreateRevId] = [binary_to_list(R) || R <- CatHistory],

  {200, R2} = req_changes("/dbs/testdb/docs?since=1"),
  A2 = jsx:decode(R2, [return_maps]),
  3 = maps:get(<<"last_seq">>, A2),
  Results2 = maps:get(<<"results">>, A2),
  2 = length(Results2),
  ok.

%%=======================================================================

accept_get_longpoll_heartbeat(_Config) ->
  Url = <<"http://localhost:7080/dbs/testdb/docs?feed=longpoll&heartbeat=10">>,
  Opts = [async, {recv_timeout, infinity}],
  LoopFun = fun(Loop, {Ref, N}=Acc) ->
                receive
                  {hackney_response, Ref, {status, StatusInt, _Reason}} ->
                    200 = StatusInt,
                    Loop(Loop, Acc);
                  {hackney_response, Ref, {headers, _Headers}} ->
                    Loop(Loop, Acc);
                  {hackney_response, Ref, done} ->
                    {ok, N};
                  {hackney_response, Ref, <<>>} ->
                    Loop(Loop, Acc);
                  {hackney_response, Ref, <<"\n">>} ->
                    Loop(Loop, {Ref, N+1});
                  {hackney_response, Ref, Bin} ->
                    R = jsx:decode(Bin,[return_maps]),
                    Results = maps:get(<<"results">>, R),
                    [_OnlyOneChange] = Results,
                    Loop(Loop, Acc);

                  Else ->
                    ct:fail("Unexpected answer from longpoll: ~p", [Else]),
                    ok
                after 2000 ->
                    {error, timeout}
                end
            end,
  Headers = [{<<"Content-Type">>, <<"application/json">>},
             {<<"A-IM">>, <<"Incremental feed">>}],

  {ok, ClientRef} = hackney:get(Url, Headers, <<>>, Opts),
  timer:sleep(100),
  CatRevId = put_cat(),
  {ok, NbHeartBeats1} = LoopFun(LoopFun, {ClientRef, 0}),
  true = NbHeartBeats1 >= 1,

  Url2 = <<Url/binary, "&since=1">>,
  {ok, ClientRef2} = hackney:get(Url2, Headers, <<>>, Opts),
  timer:sleep(100),
  delete_cat(CatRevId),
  {ok, NbHeartBeats2} =  LoopFun(LoopFun, {ClientRef2, 0}),
  true = NbHeartBeats2 >= 1,
  ok.

%%=======================================================================

accept_get_eventsource(_Config) ->
  Self = self(),
  Pid = spawn(fun () -> wait_response_from_hackney([], 5, Self) end),
  Url = <<"http://localhost:7080/dbs/testdb/docs?feed=eventsource">>,
  Opts = [async, {stream_to, Pid}],
  Headers = [{<<"Content-Type">>, <<"application/json">>},
             {<<"A-IM">>, <<"Incremental feed">>}],
  {ok, Ref} = hackney:get(Url, Headers, <<>>, Opts),
  CatRevId = put_cat(),
  delete_cat(CatRevId),
  receive
    {Ref, done} ->
      ct:fail(expected_more_data);
    {Msgs, received_as_expected} ->
      [[200, <<"OK">>], _Headers, _, _CatPut, _CatDelete] = Msgs
  after 2000 ->
      ct:fail(eventsource_timeout)
  end,
  hackney:close(Ref),
  ok.

accept_get_eventsource_headers(_Config) ->
  %% We create 3 documents
  put_anonymous(), % seq=1
  Id2 = put_anonymous(), % seq=2
  Id3 = put_anonymous(), % seq=3

  %% We start feed the change, starting since 1 (ie, changes with seq>1)
  Self = self(),
  Pid = spawn(fun () -> wait_response_from_hackney([], 5, Self) end),
  Url = <<"http://localhost:7080/dbs/testdb/docs">>,
  Opts = [async, {stream_to, Pid}],
  Headers = [{<<"Content-Accept">>, <<"text/event-stream">>},
             {<<"Last-Event-ID">>, <<"1">>},
             {<<"A-IM">>, <<"Incremental feed">>}],
  {ok, Ref} = hackney:get(Url, Headers, <<>>, Opts),

  %% We add 2 more documents
  Id4 = put_anonymous(), % seq=4
  Id5 = put_anonymous(), % seq=5

  receive
    {Ref, done} ->
      ct:fail(expected_more_data);
    {Msgs, received_as_expected} ->
      [[200, <<"OK">>], _Headers, ChangesSeq1, ChangeSeq4, ChangeSeq5] = Msgs,
      {<<"3">>, FirstChanges} = parse_event_source(ChangesSeq1),
      #{<<"last_seq">> := 3, <<"results">> := [Seq3, Seq2]} = FirstChanges,
      #{<<"seq">> := 2, <<"id">> := Id2} = Seq2,
      #{<<"seq">> := 3, <<"id">> := Id3} = Seq3,
      {<<"4">>, #{<<"results">> := [#{<<"id">>:= Id4}]}} = parse_event_source(ChangeSeq4),
      {<<"5">>, #{<<"results">> := [#{<<"id">>:= Id5}]}} = parse_event_source(ChangeSeq5)
  after 2000 ->
      ct:fail(eventsource_timeout)
  end,
  hackney:close(Ref),
  ok.

put_anonymous() ->
  Doc = "{\"name\": \"anonymous\"}",
  {201, R} = test_lib:req(post, "/dbs/testdb/docs", Doc),
  J = jsx:decode(R, [return_maps]),
  maps:get(<<"id">>, J).

wait_response_from_hackney(Msgs, 0, Parent) ->
  Parent ! {lists:reverse(Msgs), received_as_expected};
wait_response_from_hackney(Msgs, Expected, Parent) ->
  N = Expected - 1,
  receive
    {hackney_response, _Ref, {status, StatusInt, Reason}} ->
      wait_response_from_hackney([[StatusInt,Reason]|Msgs], N, Parent);
    {hackney_response, _Ref, {headers, Headers}} ->
      wait_response_from_hackney([Headers|Msgs], N, Parent);
    {hackney_response, Ref, done} ->
      Parent ! {Ref, Msgs, done},
      ok;
    {hackney_response, _Ref, Bin} ->
      wait_response_from_hackney([Bin|Msgs], N, Parent);
    _Else ->
      ok
  end.

parse_event_source(Bin) ->
  [<<"id: ", Id/binary>>, <<"data: ", Data/binary>>, _, _] =
    binary:split(Bin, <<"\n">>, [global]),
  {Id, jsx:decode(Data, [return_maps])}.

%%=======================================================================

reject_store_unknown(_Config) ->
  {400, _} = req_changes("/dbs/badstore/docs"),
  ok.

reject_bad_params(_Config) ->
  {400, _} = req_changes("/dbs/testdb/docs?badparam=whatever"),
  ok.

%%=======================================================================

req_changes(Route) ->
  Server = <<"http://localhost:7080">>,
  BinRoute = list_to_binary(Route),
  Url = << Server/binary, BinRoute/binary>>,
  Headers = [{<<"Content-Type">>, <<"application/json">>},
             {<<"A-IM">>, <<"Incremental feed">>}],
  case hackney:request(get, Url, Headers, [], []) of
    {ok, Code, _Headers, Ref} ->
      {ok, Answer} = hackney:body(Ref),
      {Code, Answer};
    Error -> Error
  end.

