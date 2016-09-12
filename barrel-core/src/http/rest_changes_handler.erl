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

-module(rest_changes_handler).

-export([init/3]).
-export([info/3]).
-export([handle/2]).
-export([terminate/3]).

-record(st, {dbname=undefined, last_seq=undefined, gen_event=undefined}).

init(_Type, Req, []) ->
    {Feed, Req2} = cowboy_req:qs_val(<<"feed">>, Req, <<"normal">>),
    init_feed(Feed, Req2).

init_feed(<<"normal">>, Req) ->
    {ok, Req, undefined};

init_feed(<<"longpoll">>, Req) ->
    {DbId, Req2} = cowboy_req:binding(dbid, Req),
    {SinceAsBin, Req3} = cowboy_req:qs_val(<<"since">>, Req2, <<"0">>),
    Since = binary_to_integer(SinceAsBin),
    {ok, GenEventPid} = subscribe(DbId),
    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    {ok, Req4} = cowboy_req:chunked_reply(200, Headers, Req3),
    State = #st{dbname=DbId, last_seq=Since, gen_event=GenEventPid},
    info(db_updated, Req4, State).


handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {IdAsBin, Req3} = cowboy_req:binding(dbid, Req2),
    handle(Method, IdAsBin, Req3, State).

handle(<<"GET">>, DbId, Req, State) ->
    {SinceAsBin, Req2} = cowboy_req:qs_val(<<"since">>, Req, <<"0">>),
    Since = binary_to_integer(SinceAsBin),
    {_, Changes} = changes(DbId, Since),
    http_reply:doc(Changes, Req2, State);

handle(_, _, Req, State) ->
    http_reply:code(405, Req, State).


info(db_updated, Req, State) ->
    DbId = State#st.dbname,
    Since = State#st.last_seq + 1,
    {LastSeq, Changes} = changes(DbId, Since),
    Json = jsx:encode(Changes),
    %% format defined by https://www.w3.org/TR/eventsource/
    ok = cowboy_req:chunk(["id: ", id(), "\ndata: ", Json, "\n\n"], Req),
    {loop, Req, State#st{last_seq=LastSeq}}.

terminate(_Reason, _Req, #st{dbname=DbName}) ->
    %% TODO improve closing of streamed changes
    %% by default, cowboy does not close connection
    %% this will never be called as we will receive
    %% a continuous flow of database update
    %% Maybe add a timeout in the change_events_handler?
    ok = unsubsribe(DbName),
    ok;

terminate(_Reason, _Req, _State) ->
    ok.


%% ----------


changes(DbId, Since) ->
    Fun = fun(Seq, DocInfo, _Doc, {_LastSeq, DocInfos}) ->
                  {ok, {Seq, [DocInfo|DocInfos]}}
          end,
    {LastSeq, Changes} = barrel_db:changes_since(DbId, Since, Fun, {Since, []}),
    {LastSeq, #{<<"last_seq">> => LastSeq,
                <<"results">> => Changes}}.


id() ->
    {Mega, Sec, Micro} = erlang:timestamp(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(Id, 16).

subscribe(DbName) ->
    Key = key(DbName),
    {ok, Pid} = gen_event:start_link({via, gproc, Key}),
    ok = gen_event:add_handler({via, gproc, Key}, change_events_handler, self()),
    {ok, Pid}.

unsubsribe(DbName) ->
    Key = key(DbName),
    ok = gen_event:stop(Key),
    ok.


key(DbName) ->
    {n, l, {ev, DbName}}.
