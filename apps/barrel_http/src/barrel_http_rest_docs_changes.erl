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

-module(barrel_http_rest_docs_changes).
-author("Bernard Notarianni").

-export([init/3]).
-export([info/3]).
-export([handle/2]).
-export([terminate/3]).

-include("barrel_http_rest_docs.hrl").


init(_Type, Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

route(Req, #state{method= <<"GET">>}=State) ->
  check_database_db(Req, State);
route(Req, State) ->
  {ok, Req2, State} = barrel_http_reply:error(405, Req, []),
  {shutdown, Req2, State}.

check_database_db(Req, State) ->
  {Database, Req2} = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    true ->
      check_params(Req2, State#state{database=Database});
    _Error ->
      lager:info("unknown database requested: ~p~n", [Database]),
      {ok, Req3, S} = barrel_http_reply:error(400, "unknown database", Req2, State),
      {shutdown, Req3, S}
  end.


check_params(Req, State) ->
  StateDefault = State#state{feed=normal, since=0},
  {Params, Req2} = cowboy_req:qs_vals(Req),
  case parse_params(Params, StateDefault) of
    {error, {unknown_param, _}} ->
      {ok, Req3, S} = barrel_http_reply:error(400, "unknown parameter", Req2, State),
      {shutdown, Req3, S};
    {ok, State2} ->
      check_eventsource_headers(Req2, State2)
  end.

parse_params([], State) ->
  {ok, State};
parse_params([{<<"feed">>, FeedBin}|Tail], State) ->
  Feed = binary_to_atom(FeedBin, utf8),
  parse_params(Tail, State#state{feed=Feed});
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
  {ContentTypeBin, Req2} = cowboy_req:header(<<"accept">>, Req, <<"undefined">>),
  {LastEventIdBin, Req3} = cowboy_req:header(<<"last-event-id">>, Req2, <<"undefined">>),
  ContentType = string:to_lower(binary_to_list(ContentTypeBin)),
  route_evensource_headers(ContentType, LastEventIdBin, Req3, State).

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
  Headers = [],
  {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
  {ok, Req2, S};

init_feed_changes(Req, #state{feed=longpoll}=S) ->
  Headers = [],
  {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
  {ok, Req3, S2} = init_hearbeat(Req2, S),
  ok = barrel_event:reg(S2#state.database),
  {loop, Req3, S2#state{subscribed=true}};

init_feed_changes(Req, #state{feed=eventsource}=S) ->
  Headers = [{<<"content-type">>, <<"text/event-stream">>}],
  {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
  LastSeq = reply_eventsource_chunks(S#state.database, S#state.last_seq, S#state.options, Req2),
  ok = barrel_event:reg(S#state.database),
  {ok, Req3, S2} = init_hearbeat(Req2, S),
  {loop, Req3, S2#state{last_seq=LastSeq, subscribed=true}}.

init_hearbeat(Req, State) ->
  {HeartBeatBin, Req2} = cowboy_req:qs_val(<<"heartbeat">>, Req, <<"60000">>),
  HeartBeat = binary_to_integer(HeartBeatBin),
  {ok, Timer} = timer:send_interval(HeartBeat, self(), heartbeat),
  {ok, Req2, State#state{timer=Timer, heartbeat=HeartBeat}}.


handle(Req, S) ->
  Database = S#state.database,
  Since = S#state.since,
  Options = S#state.options,

  %% start the initial chunk
  ok = cowboy_req:chunk(<<"{\"changes\":[">>, Req),
  Fun =
    fun
      (Change, {PreviousLastSeq, Pre}) ->
        Seq = maps:get(seq, Change),
        Chunk = <<  Pre/binary, (jsx:encode(Change))/binary>>,
        ok = cowboy_req:chunk(Chunk, Req),
        LastSeq = max(Seq, PreviousLastSeq),
        {ok, {LastSeq, <<",">>}}
    end,
  {LastSeq, _} = barrel_local:changes_since(Database, Since, Fun, {Since, <<"">>}, Options),

  %% close the document list and return the calculated count
  ok = cowboy_req:chunk(
         iolist_to_binary([
                           <<"],">>,
                           <<"\"last_seq\":">>,
                           integer_to_binary(LastSeq),
                           <<"}">>
                          ]),
         Req
        ),
  {ok, Req, S}.

info(heartbeat, Req, S) ->
  ok = cowboy_req:chunk(<<"\n">>, Req),
  {loop, Req, S};

info({'$barrel_event', FromDbId, db_updated}, Req, #state{database=FromDbId, feed=longpoll}=S) ->
  handle(Req, S);

info({'$barrel_event', FromDbId, db_updated}, Req, #state{database=FromDbId, feed=eventsource}=S) ->
  LastSeq = reply_eventsource_chunks(S#state.database, S#state.last_seq, S#state.options, Req),
  {loop, Req, S#state{last_seq=LastSeq}};

info(_Info, Req, S) ->
  {loop, Req, S}.


terminate(_Reason, _Req, State) ->
  terminate_subscription(State),
  terminate_timer(State),
  ok.

terminate_subscription(#state{subscribed=true}) ->
  %% TODO improve closing of streamed changes
  %% by default, cowboy does not close connection
  %% this will never be called as we will receive
  %% a continuous flow of database update
  %% Maybe add a timeout in the change_events_handler?
  ok = barrel_event:unreg(),
  ok;
terminate_subscription(_) ->
  ok.

terminate_timer(#state{timer=undefined}) ->
  ok;
terminate_timer(#state{timer=Timer}) ->
  {ok, cancel} = timer:cancel(Timer),
  ok.


reply_eventsource_chunks(Database, Since, Options, Req) ->
  Fun =
    fun
      (Change, {PreviousLastSeq, Pre}) ->
        Seq = maps:get(seq, Change),
        Chunk = << "id: ", (integer_to_binary(Seq))/binary, "\n",
                   "data: ", Pre/binary, (jsx:encode(Change))/binary, "\n",
                   "\n">>,
        ok = cowboy_req:chunk(Chunk, Req),
        LastSeq = max(Seq, PreviousLastSeq),
        {ok, {LastSeq, <<"">>}}
    end,
  {LastSeq, _} = barrel_local:changes_since(Database, Since, Fun, {Since, <<"">>}, Options),
  LastSeq.
