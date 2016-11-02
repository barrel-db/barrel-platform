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



-record(state, {store, dbid, changes, last_seq, feed, subscribed}).

init(_Type, Req, []) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  {FeedBin, Req4} = cowboy_req:qs_val(<<"feed">>, Req3, <<"normal">>),
  Feed = binary_to_atom(FeedBin, utf8),
  {SinceBin, Req5} = cowboy_req:qs_val(<<"since">>, Req4, <<"0">>),
  Since = binary_to_integer(SinceBin),
  {LastSeq, Changes} = changes(Store, DbId, Since),
  S = #state{store=Store, dbid=DbId, changes=Changes, last_seq=LastSeq, feed=Feed},
  init_feed(Req5, S).



init_feed(Req, #state{feed=normal}=S) ->
  {ok, Req, S};

init_feed(Req, #state{feed=longpoll, changes=[]}=S) ->
  DbId = S#state.dbid,
  ok = barrel_event:reg(DbId),
  info({'$barrel_event', DbId, db_updated}, Req, S#state{subscribed=true});

init_feed(Req, #state{feed=longpoll}=S) ->
  {ok, Req, S};

init_feed(Req, #state{feed=eventsource}=S) ->
  DbId = S#state.dbid,
  ok = barrel_event:reg(DbId),
  Headers = [{<<"content-type">>, <<"text/event-stream">>}],
  {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
  info({'$barrel_event', DbId, db_updated}, Req2, S#state{subscribed=true}).



handle(Req, S) ->
  {Method, Req2} = cowboy_req:method(Req),
  handle(Method, Req2, S).

handle(<<"GET">>, Req, S) ->
  LastSeq = S#state.last_seq,
  Changes = S#state.changes,
  Json = to_json(LastSeq, Changes),
  barrel_http_reply:json(Json, Req, S);

handle(_, Req, S) ->
  barrel_http_reply:code(405, Req, S).



info({'$barrel_event', _FromDbId, db_updated}, Req, S) ->
  Store = S#state.store,
  DbId = S#state.dbid,
  Since = S#state.last_seq,
  {LastSeq, Changes} = changes(Store, DbId, Since),
  db_updated(Changes, LastSeq, Req, S).

db_updated([], LastSeq, Req, #state{feed=longpoll}=S) ->
  {loop, Req, S#state{last_seq=LastSeq, changes=[]}};

db_updated(Changes, LastSeq, Req, #state{feed=longpoll}=S) ->
  Json = to_json(LastSeq, Changes),
  barrel_http_reply:json(Json, Req, S);

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

changes(Store, DbId, Since) ->
  Fun = fun(Seq, DocInfo, _Doc, {_LastSeq, DocInfos}) ->
            {ok, {Seq, [DocInfo|DocInfos]}}
        end,
  {ok, Conn} = barrel_http_conn:peer(Store, DbId),
  barrel_db:changes_since(Conn, Since, Fun, {Since, []}).

to_json(LastSeq, Changes) ->
  Map = #{<<"last_seq">> => LastSeq,
          <<"results">> => Changes},
  jsx:encode(Map).


id() ->
  {Mega, Sec, Micro} = erlang:timestamp(),
  Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
  integer_to_list(Id, 16).

