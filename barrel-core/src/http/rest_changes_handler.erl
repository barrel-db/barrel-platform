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

-record(st, {dbname, changes, last_seq, mode}).

init(_Type, Req, []) ->
    {Feed, Req2} = cowboy_req:qs_val(<<"feed">>, Req, <<"normal">>),
    {DbId, Req3} = cowboy_req:binding(dbid, Req2),
    {SinceAsBin, Req3} = cowboy_req:qs_val(<<"since">>, Req3, <<"0">>),
    Since = binary_to_integer(SinceAsBin),
    init_feed(Feed, DbId, Since, Req3).




init_feed(<<"normal">>, DbId, Since, Req) ->
    {LastSeq, Changes} = changes(DbId, Since),
    State = #st{dbname=DbId, changes=Changes, last_seq=LastSeq, mode=normal},
    {ok, Req, State};

init_feed(<<"longpoll">>, DbId, Since, Req) ->
    {LastSeq, Changes} = changes(DbId, Since),
    State = #st{dbname=DbId, changes=Changes, last_seq=LastSeq, mode=longpoll},
    case Changes of
        [] ->
            ok = subscribe(DbId),
            info(db_updated, Req, State);
        _ ->
            {ok, Req, State}
    end;

init_feed(<<"eventsource">>, DbId, Since, Req) ->
    ok = subscribe(DbId),
    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
    State = #st{dbname=DbId, last_seq=Since, mode=eventsource},
    info(db_updated, Req2, State).




handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    handle(Method, Req2, State).

handle(<<"GET">>, Req, State) ->
    LastSeq = State#st.last_seq,
    Changes = State#st.changes,
    Json = to_json(LastSeq, Changes),
    http_reply:json(Json, Req, State);

handle(_, Req, State) ->
    http_reply:code(405, Req, State).



info(db_updated, Req, State) ->
    DbId = State#st.dbname,
    Since = State#st.last_seq,
    {LastSeq, Changes} = changes(DbId, Since),
    case State#st.mode of
        longpoll ->
            case Changes of
                [] ->
                    info(db_updated, Req, State);
                _ ->
                    {ok, Req, State#st{last_seq=LastSeq, changes=Changes}}
            end;
        eventsource ->
            Json = to_json(LastSeq, Changes),
            %% format defined by https://www.w3.org/TR/eventsource/
            ok = cowboy_req:chunk(["id: ", id(), "\ndata: ", Json, "\n\n"], Req),
            {loop, Req, State#st{last_seq=LastSeq}}
    end.



terminate(_Reason, _Req, #st{dbname=DbId}) ->
    %% TODO improve closing of streamed changes
    %% by default, cowboy does not close connection
    %% this will never be called as we will receive
    %% a continuous flow of database update
    %% Maybe add a timeout in the change_events_handler?
    ok = unsubscribe(DbId),
    ok;
terminate(_Reason, _Req, _State) ->
    ok.


%% ----------

changes(DbId, Since) ->
    Fun = fun(Seq, DocInfo, _Doc, {_LastSeq, DocInfos}) ->
                  {ok, {Seq, [DocInfo|DocInfos]}}
          end,
    barrel_db:changes_since(DbId, Since, Fun, {Since, []}).
    %% {LastSeq, Changes} = barrel_db:changes_since(DbId, Since, Fun, {Since, []}),
    %% {LastSeq, #{<<"last_seq">> => LastSeq,
    %%             <<"results">> => Changes}}.

to_json(LastSeq, Changes) ->
    Map = #{<<"last_seq">> => LastSeq,
            <<"results">> => Changes},
    jsx:encode(Map).


id() ->
    {Mega, Sec, Micro} = erlang:timestamp(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(Id, 16).

subscribe(DbName) ->
    Key = barrel_db_event:key(DbName),
    ok = gen_event:add_handler({via, gproc, Key}, change_events_handler, self()),
    ok.

unsubscribe(DbName) ->
    Key = barrel_db_event:key(DbName),
    ok = gen_event:delete_handler({via, gproc, Key}, change_events_handler, self()),
    ok.

