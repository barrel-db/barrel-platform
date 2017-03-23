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

-module(barrel_http_rest_docs_changes).
-author("Bernard Notarianni").

-export([init/2]).
-export([info/3]).
-export([handle/2]).
-export([terminate/3]).

-include("barrel_http_rest_docs.hrl").


init(Req, State) ->
  Method = cowboy_req:method(Req),
  route(Req, State#state{method=Method}).

route(Req, #state{method= <<"GET">>}=State) ->
  check_database_db(Req, State);
route(Req, State) ->
  {ok, Req2, State} = barrel_http_reply:error(405, Req, []),
  {shutdown, Req2, State}.

check_database_db(Req, State) ->
  Database = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    true ->
      check_params(Req, State#state{database=Database});
    _Error ->
      _ = lager:info("unknown database requested: ~p~n", [Database]),
      {ok, Req3, S} = barrel_http_reply:error(400, "unknown database", Req, State),
      {shutdown, Req3, S}
  end.


check_params(Req, State) ->
  StateDefault = State#state{feed=normal, since=0},
  Params = cowboy_req:parse_qs(Req),
  case parse_params(Params, StateDefault) of
    {error, {unknown_param, _}} ->
      {ok, Req2, S} = barrel_http_reply:error(400, "unknown parameter", Req, State),
      {shutdown, Req2, S};
    {ok, State2} ->
      check_eventsource_headers(Req, State2)
  end.

parse_params([], State) ->
  {ok, State};
parse_params([{<<"since">>, SinceBin}|Tail], State) ->
  Since = binary_to_integer(SinceBin),
  parse_params(Tail, State#state{since=Since});
parse_params([{<<"heartbeat">>, HeartBeatBin}|Tail], State) ->
  HeartBeat = binary_to_integer(HeartBeatBin),
  parse_params(Tail, State#state{heartbeat=HeartBeat});
parse_params([{<<"history">>, <<"all">>}|Tail], State=#state{options=Options}) ->
  parse_params(Tail, State#state{options=[{history, all} | Options]});
parse_params([{<<"include_doc">>, <<"true">>}|Tail], State=#state{options=Options}) ->
  parse_params(Tail, State#state{options=[{include_doc, true} | Options]});
parse_params([{Param, _Value}|_], _State) ->
  {error, {unknown_param, Param}}.


check_eventsource_headers(Req, State) ->
  ContentTypeBin = cowboy_req:header(<<"accept">>, Req, <<"undefined">>),
  LastEventIdBin = cowboy_req:header(<<"last-event-id">>, Req, <<"undefined">>),
  ContentType = string:to_lower(binary_to_list(ContentTypeBin)),
  route_evensource_headers(ContentType, LastEventIdBin, Req, State).

route_evensource_headers("text/event-stream", LastEventId, Req, State) ->
  Since = case LastEventId of
            <<"undefined">> -> 0;
            Integer -> binary_to_integer(Integer)
          end,
  init_feed(Req, State#state{feed=eventsource, since=Since});
route_evensource_headers(_,_, Req, State) ->
  init_feed(Req, State).


init_feed(Req, #state{database=_Database, since=Since, options=_Options}=State) ->
  init_feed_changes(Req, State#state{last_seq=Since}).

init_feed_changes(Req, #state{feed=normal}=S) ->
  Req2 = cowboy_req:stream_reply(200, Req),
  {ok, Req2, S};

init_feed_changes(Req, #state{feed=eventsource, options=Options}=S) ->
  Self = self(),
  Callback =
    fun(Change) ->
        Self ! {change, Change}
    end,
  Source = S#state.database,
  Since = S#state.since,
  IncludeDoc = proplists:get_value(include_doc, Options, false),
  SseOptions = #{since => Since, mode => sse, changes_cb => Callback, include_doc => IncludeDoc },
  {ok, Pid} = barrel_local_changes:start_link(Source, SseOptions),

  Req3 = cowboy_req:stream_reply(200, #{<<"content">> => <<"text/event-stream">>}, Req),
  {ok, Req4, S2} = init_hearbeat(Req3, S),
  {cowboy_loop, Req4, S2#state{changes_since_pid=Pid}}.

init_hearbeat(Req, State) ->
  #{heartbeat := HeartBeatBin}
    = cowboy_req:match_qs([{heartbeat, [], <<"60000">>}], Req),
  HeartBeat = binary_to_integer(HeartBeatBin),
  {ok, Timer} = timer:send_interval(HeartBeat, self(), heartbeat),
  {ok, Req, State#state{timer=Timer, heartbeat=HeartBeat}}.


handle(Req, S) ->
  Database = S#state.database,
  Since = S#state.since,
  Options = S#state.options,

  %% start the initial chunk
  ok = cowboy_req:stream_body(<<"{\"changes\":[">>, nofin, Req),
  Fun =
    fun
      (Change, {PreviousLastSeq, Pre}) ->
        Seq = maps:get(<<"seq">>, Change),
        Chunk = <<  Pre/binary, (jsx:encode(Change))/binary>>,
        ok = cowboy_req:stream_body(Chunk, nofin, Req),
        LastSeq = max(Seq, PreviousLastSeq),
        {ok, {LastSeq, <<",">>}}
    end,
  {LastSeq, _} = barrel_local:changes_since(Database, Since, Fun, {Since, <<"">>}, Options),

  %% close the document list and return the calculated count
  ok = cowboy_req:stream_body(
         iolist_to_binary([
                           <<"],">>,
                           <<"\"last_seq\":">>,
                           integer_to_binary(LastSeq),
                           <<"}">>
                          ]),
         fin, Req
        ),
  {ok, Req, S}.

info(heartbeat, Req, S) ->
  ok = cowboy_req:stream_body(<<"\n">>, nofin, Req),
  {ok, Req, S};

info({change, Change}, Req, #state{feed=eventsource}=S) ->
  Seq = maps:get(<<"seq">>, Change),
  Chunk = << "id: ", (integer_to_binary(Seq))/binary, "\n",
             "data: ", (jsx:encode(Change))/binary, "\n",
             "\n">>,
  ok = cowboy_req:stream_body(Chunk, nofin, Req),
  {ok, Req, S#state{last_seq=Seq}};

info(_Info, Req, S) ->
  {ok, Req, S}.


terminate(_Reason, _Req, State) ->
  terminate_timer(State),
  ok.

terminate_timer(#state{timer=undefined}) ->
  ok;
terminate_timer(#state{timer=Timer}) ->
  {ok, cancel} = timer:cancel(Timer),
  ok.
