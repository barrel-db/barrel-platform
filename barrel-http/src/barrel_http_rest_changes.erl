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

-module(barrel_http_rest_changes).
-author("Bernard Notarianni").

-export([init/3]).
-export([info/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
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
                   ,#{ name => <<"dbid">>
                     , description => <<"Database ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>
                     }
                   ,#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>
                     }
                   ]
               }
     },
  [trails:trail("/:store/:dbid/_changes", ?MODULE, [], Metadata)].



-record(state, {method, conn, store, dbid, feed, since, changes, last_seq, subscribed, heartbeat}).

init(_Type, Req, []) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, #state{method=Method}).

route(Req, #state{method= <<"GET">>}=State) ->
  check_store_db(Req, State);
route(Req, State) ->
  {ok, Req2, State} = barrel_http_reply:error(405, Req, []),
  {shutdown, Req2, State}.

check_store_db(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  case barrel:connect_database(barrel_lib:to_atom(Store), DbId) of
    {ok, Conn} ->
      State2 = State#state{store=Store, dbid=DbId, conn=Conn},
      check_params(Req3, State2);
    _Error ->
      {ok, Req4, S} = barrel_http_reply:error(400, "db or database not found", Req, State),
      {shutdown, Req4, S}
  end.

check_params(Req, State) ->
  StateDefault = State#state{feed=normal, since=0},
  {Params, Req2} = cowboy_req:qs_vals(Req),
  case parse_params(Params, StateDefault) of
    {error, {unknown_param, _}} ->
      {ok, Req3, S} = barrel_http_reply:error(400, "unknown parameter", Req2, State),
      {shutdown, Req3, S};
    {ok, State2} ->
      init_feed(Req2, State2)
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
parse_params([{Param, _Value}|_], _State) ->
  {error, {unknown_param, Param}}.



init_feed(Req, #state{conn=Conn, since=Since}=State) ->
  {LastSeq, Changes} = changes(Conn, Since),
  init_feed_changes(Req, State#state{changes=Changes, last_seq=LastSeq}).

init_feed_changes(Req, #state{feed=normal}=S) ->
  {ok, Req, S};

init_feed_changes(Req, #state{feed=longpoll, changes=[]}=S) ->
  %% No changes available for reply. We register for db_updated events.
  ok = barrel_event:reg(S#state.conn),
  {ok, Req2, S2} = init_chunked_reply_with_hearbeat([], Req, S),
  {loop, Req2, S2#state{subscribed=true}};

init_feed_changes(Req, #state{feed=longpoll}=S) ->
  %% Changes available. We return them immediatly.
  {ok, Req, S};

init_feed_changes(Req, #state{feed=eventsource}=S) ->
  ok = barrel_event:reg(S#state.conn),
  #{ store := Store, name := Name} = S#state.conn,
  Headers = [{<<"content-type">>, <<"text/event-stream">>}],
  {ok, Req2, S2} = init_chunked_reply_with_hearbeat(Headers, Req, S),
  info({'$barrel_event', {Store, Name}, db_updated}, Req2, S2#state{subscribed=true}).

init_chunked_reply_with_hearbeat(Headers, Req, State) ->
  {HeartBeatBin, Req2} = cowboy_req:qs_val(<<"heartbeat">>, Req, <<"60000">>),
  HeartBeat = binary_to_integer(HeartBeatBin),
  timer:send_interval(HeartBeat, self(), heartbeat),
  {ok, Req3} = cowboy_req:chunked_reply(200, Headers, Req2),
  {ok, Req3, State#state{heartbeat=HeartBeat}}.


handle(Req, S) ->
  LastSeq = S#state.last_seq,
  Changes = S#state.changes,
  Json = to_json(LastSeq, Changes),
  barrel_http_reply:json(Json, Req, S).

info(heartbeat, Req, S) ->
  ok = cowboy_req:chunk(<<"\n">>, Req),
  {loop, Req, S};

info({'$barrel_event', _FromDbId, db_updated}, Req, S) ->
  {LastSeq, Changes} = changes(S#state.conn, S#state.last_seq),
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
  ok = cowboy_req:chunk(["id: ", id(), "\ndata: ", Json, "\n\n"], Req),
  {loop, Req, S#state{last_seq=LastSeq}}.



terminate(_Reason, _Req, #state{subscribed=true}) ->
  %% TODO improve closing of streamed changes
  %% by default, cowboy does not close connection
  %% this will never be called as we will receive
  %% a continuous flow of database update
  %% Maybe add a timeout in the change_events_handler?
  ok = barrel_event:unreg(),
  ok;

terminate(_Reason, _Req, _S) ->
  ok.


%% ----------

changes(Conn, Since) ->
  Fun = fun(Seq, DocInfo, _Doc, {_LastSeq, DocInfos}) ->
            {ok, {Seq, [DocInfo|DocInfos]}}
        end,
  barrel:changes_since(Conn, Since, Fun, {Since, []}).

to_json(LastSeq, Changes) ->
  Map = #{<<"last_seq">> => LastSeq,
          <<"results">> => Changes},
  jsx:encode(Map).


id() ->
  {Mega, Sec, Micro} = erlang:timestamp(),
  Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
  integer_to_list(Id, 16).

