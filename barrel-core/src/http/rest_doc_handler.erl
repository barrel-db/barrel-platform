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

-module(rest_doc_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {DbIdAsBin, Req3} = cowboy_req:binding(dbid, Req2),
    {DocIdAsBin, Req4} = cowboy_req:binding(docid, Req3),
    handle(Method, DbIdAsBin, DocIdAsBin, Req4, State).

handle(<<"GET">>, DbId, DocIdAsBin, Req, State) ->
    case barrel_db:get(DbId, DocIdAsBin, []) of
        {error, not_found} -> http_reply:code(404, Req, State);
        {ok, Doc} -> http_reply:doc(Doc, Req, State)
    end;

handle(<<"PUT">>, DbId, DocIdAsBin, Req, State) ->
    {ok, [{Body, _}], Req2} = cowboy_req:body_qs(Req),
    Json = jsx:decode(Body),
    Doc = maps:from_list(Json),
    case barrel_db:put(DbId, DocIdAsBin, Doc, []) of
        {error, {conflict, doc_exists}} ->
            http_reply:code(409, Req2, State);
        {ok, _DocIdAsBin, RevId} ->
            Reply = #{<<"ok">> => true,
                      <<"id">> => DocIdAsBin,
                      <<"rev">> => RevId},
            http_reply:doc(Reply, Req2, State)
    end;

handle(<<"DELETE">>, DbId, DocId, Req, State) ->
    case cowboy_req:qs_val(<<"rev">>, Req) of
        {undefined, Req2} ->
            http_reply:code(400, Req2, State);
        {RevId, Req2} ->
            delete(DbId, DocId, RevId, Req2, State)
    end;


handle(_, _, _, Req, State) ->
    http_reply:code(405, Req, State).



delete(DbId, DocId, RevId, Req, State) ->
    {ok, DocId, RevId2} = barrel_db:delete(DbId, DocId, RevId, []),
    Reply = #{<<"ok">> => true,
              <<"id">> => DocId,
              <<"rev">> => RevId2},
    http_reply:doc(Reply, Req, State).

