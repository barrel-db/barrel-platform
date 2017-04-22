%% Copyright 2017, Bernard Notarianni
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
        , accept_get_eventsource_headers/1
        , reject_store_unknown/1
        , reject_bad_params/1
        ]).

all() -> [ accept_get_normal
         , accept_get_history_all
         , accept_get_eventsource_headers
         , reject_store_unknown
         , reject_bad_params
         ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel_local:delete_db(<<"testdb">>),
  Config.

end_per_suite(Config) ->
  application:stop(barrel),
  Config.

r(Req) ->
  test_lib:req(Req).

post_cat() ->
  Doc = #{<<"id">> => <<"cat">>, <<"name">> => <<"tom">>},
  {ok, _, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  RevId.

delete_cat(CatRevId) ->
  {ok, _, RevId} = barrel_local:delete(<<"testdb">>, <<"cat">>, [{rev, CatRevId}]),
  RevId.

post_dog() ->
  Doc = #{<<"id">> => <<"dog">>, <<"name">> => <<"spike">>},
  {ok, _, RevId} = barrel_local:post(<<"testdb">>, Doc, []),
  RevId.


%%=======================================================================

accept_get_normal(_Config) ->
  post_cat(),
  post_dog(),

  {200, R1} = req_changes("/dbs/testdb/docs"),
  A1 = jsx:decode(R1, [return_maps]),
  2 = maps:get(<<"last_seq">>, A1),
  Results1 = maps:get(<<"changes">>, A1),
  2 = length(Results1),

  {200, R2} = req_changes("/dbs/testdb/docs?since=1"),
  A2 = jsx:decode(R2, [return_maps]),
  2 = maps:get(<<"last_seq">>, A2),
  Results2 = maps:get(<<"changes">>, A2),
  1 = length(Results2),
  ok.

accept_get_history_all(_Config) ->
  CreateRevId = post_cat(),
  post_dog(),
  DeleteRevId = delete_cat(CreateRevId),

  #{code := 200,
    doc := A1} = r(#{method => get,
                     headers => [{"A-IM", "Incremental feed"}],
                     route => "/dbs/testdb/docs?history=all"}),

  3 = maps:get(<<"last_seq">>, A1),
  Results1 = maps:get(<<"changes">>, A1),
  2 = length(Results1),
  [_, #{<<"id">> := <<"cat">>, <<"changes">> := CatHistory}] = Results1,
  [DeleteRevId, CreateRevId] = CatHistory,

  #{code := 200,
    doc := A2} = r(#{method => get,
                     headers => [{"A-IM", "Incremental feed"}],
                     route => "/dbs/testdb/docs?since=1"}),

  3 = maps:get(<<"last_seq">>, A2),
  Results2 = maps:get(<<"changes">>, A2),
  2 = length(Results2),
  ok.

%%=======================================================================

accept_get_eventsource_headers(_Config) ->
  Url = <<"http://localhost:7080/dbs/testdb/docs">>,
  Headers = [{<<"Accept">>, <<"text/event-stream">>},
             {<<"Last-Event-ID">>, <<"1">>},
             {<<"A-IM">>, <<"Incremental feed">>}],
  test_eventsource(Url, Headers).


test_eventsource(Url, Headers) ->
  %% We create 3 documents
  _Id1 = post_anonymous(), % seq=1
  Id2 = post_anonymous(), % seq=2
  Id3 = post_anonymous(), % seq=3

  %% We start feed the change, starting since 1 (ie, changes with seq>1)
  Opts = [async],
  {ok, Ref} = hackney:get(Url, Headers, <<>>, Opts),

  %% We add 2 more documents
  Id4 = post_anonymous(), % seq=4
  Id5 = post_anonymous(), % seq=5

  Msgs = collect_msgs_from_hackney([], 6),
  lager:info("messages are ~p~n", [Msgs]),
  [[200, <<"OK">>], _Headers, ChangeSeq2, ChangeSeq3, ChangeSeq4, ChangeSeq5] = Msgs,

  {<<"2">>, #{<<"id">> := Id2}} = parse_event_source(ChangeSeq2),
  {<<"3">>, #{<<"id">> := Id3}} = parse_event_source(ChangeSeq3),
  {<<"4">>, #{<<"id">> := Id4}} = parse_event_source(ChangeSeq4),
  {<<"5">>, #{<<"id">> := Id5}} = parse_event_source(ChangeSeq5),
  hackney:close(Ref),
  ok.

post_anonymous() ->
  Doc = "{\"name\": \"anonymous\"}",
  {201, R} = test_lib:req(post, "/dbs/testdb/docs", Doc),
  J = jsx:decode(R, [return_maps]),
  maps:get(<<"id">>, J).

collect_msgs_from_hackney(Msgs, 0) ->
  lists:reverse(Msgs);
collect_msgs_from_hackney(Msgs, N) ->
  receive
    {hackney_response, _Ref, {status, StatusInt, Reason}} ->
      collect_msgs_from_hackney([[StatusInt,Reason]|Msgs], N-1);
    {hackney_response, _Ref, {headers, Headers}} ->
      collect_msgs_from_hackney([Headers|Msgs], N-1);
    {hackney_response, _Ref, done} ->
      collect_msgs_from_hackney([done|Msgs], N);
    {hackney_response, _Ref, <<"\n">>} ->
      collect_msgs_from_hackney(Msgs, N);
    {hackney_response, _Ref, Bin} ->
      lager:info("collected ~p~n", [Bin]),
      collect_msgs_from_hackney([Bin|Msgs], N-1);
    Else ->
      {error, {unexpected_message, Else}}
  after 2000 ->
      {error, hackney_timeout_in_test}
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

