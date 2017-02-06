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

-export([trails/1]).

-include("barrel_http_rest_docs.hrl").

trails(Module) ->
  Metadata =
    #{ get => #{ summary => "Get changes which happened on the database."
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"feed">>
                     , description => <<"longpoll/eventsource reply">>
                     , in => <<"query">>
                     , required => false
                     , type => <<"string">>
                     , enum => [ <<"normal">>
                               , <<"longpoll">>
                               , <<"eventsource">>
                               ]
                     }
                   ,#{ name => <<"since">>
                     , description => <<"Starting sequence">>
                     , in => <<"path">>
                     , required => false
                     , type => <<"integer">>
                     }
                   ,#{ name => <<"database">>
                     , description => <<"Database ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>
                     }
                   ]
               }
     },
  [trails:trail("/dbs/:database/docs/_changes", Module, [], Metadata)].


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
parse_params([{<<"history">>, <<"all">>}|Tail], State) ->
  parse_params(Tail, State#state{options=[{history, all}]});
parse_params([{Param, _Value}|_], _State) ->
  {error, {unknown_param, Param}}.


check_eventsource_headers(Req, State) ->
  {ContentTypeBin, Req2} = cowboy_req:header(<<"content-accept">>, Req, <<"undefined">>),
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


init_feed(Req, #state{database=Database, since=Since, options=Options}=State) ->
  {LastSeq, Changes} = changes(Database, Since, Options),
  init_feed_changes(Req, State#state{changes=Changes, last_seq=LastSeq}).

init_feed_changes(Req, #state{feed=normal}=S) ->
  {ok, Req, S};

init_feed_changes(Req, #state{feed=longpoll, changes=[]}=S) ->
  %% No changes available for reply. We register for db_updated events.
  ok = barrel_event:reg(S#state.database),
  {ok, Req2, S2} = init_chunked_reply_with_hearbeat([], Req, S),
  {loop, Req2, S2#state{subscribed=true}};

init_feed_changes(Req, #state{feed=longpoll}=S) ->
  %% Changes available. We return them immediatly.
  {ok, Req, S};

init_feed_changes(Req, #state{feed=eventsource}=S) ->
  ok = barrel_event:reg(S#state.database),
  Headers = [{<<"content-type">>, <<"text/event-stream">>}],
  {ok, Req2, S2} = init_chunked_reply_with_hearbeat(Headers, Req, S),
  Changes = S#state.changes,
  LastSeq = S#state.last_seq,
  db_updated(Changes, LastSeq, Req2, S2).

init_chunked_reply_with_hearbeat(Headers, Req, State) ->
  {HeartBeatBin, Req2} = cowboy_req:qs_val(<<"heartbeat">>, Req, <<"60000">>),
  HeartBeat = binary_to_integer(HeartBeatBin),
  Timer = timer:send_interval(HeartBeat, self(), heartbeat),
  {ok, Req3} = cowboy_req:chunked_reply(200, Headers, Req2),
  {ok, Req3, State#state{timer=Timer, heartbeat=HeartBeat}}.


handle(Req, S) ->
  LastSeq = S#state.last_seq,
  Changes = S#state.changes,
  Json = to_json(LastSeq, Changes),
  barrel_http_reply:json(Json, Req, S).

info(heartbeat, Req, S) ->
  ok = cowboy_req:chunk(<<"\n">>, Req),
  {loop, Req, S};

info({'$barrel_event', FromDbId, db_updated}, Req, #state{database=FromDbId}=S) ->
  {LastSeq, Changes} = changes(S#state.database, S#state.last_seq, S#state.options),
  db_updated(Changes, LastSeq, Req, S);
info(_Info, Req, S) ->
  {loop, Req, S}.

db_updated([], LastSeq, Req, #state{feed=longpoll}=S) ->
  {loop, Req, S#state{last_seq=LastSeq, changes=[]}};

db_updated(Changes, LastSeq, Req, #state{feed=longpoll}=S) ->
  Json = to_json(LastSeq, Changes),
  ok = cowboy_req:chunk(Json, Req),
  {ok, Req, S};

db_updated(Changes, LastSeq, Req, #state{feed=eventsource}=S) ->
  Json = to_json(LastSeq, Changes),
  %% format defined by https://www.w3.org/TR/eventsource/
  ok = cowboy_req:chunk(["id: ", integer_to_list(LastSeq),
                         "\ndata: ", Json, "\n\n"], Req),
  {loop, Req, S#state{last_seq=LastSeq}}.


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

%% ----------

changes(Database, Since, Options) ->
  Fun = fun(Seq, Change, {PreviousLastSeq, Changes1}) ->
            LastSeq = max(Seq, PreviousLastSeq),
            {ok, {LastSeq, [Change|Changes1]}}
        end,
  {LastSeq, Changes} = barrel:changes_since(Database, Since, Fun, {Since, []}, Options),
  {LastSeq, lists:reverse(Changes)}.
to_json(LastSeq, Changes) ->
  Map = #{<<"last_seq">> => LastSeq,
          <<"changes">> => Changes},
  jsx:encode(Map).
