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

-module(barrel_http_rest_revsdiff).
-author("Bernard Notarianni").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


init(_Type, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {Database, Req3} = cowboy_req:binding(database, Req2),
  handle(Method, Database, Req3, State).

handle(<<"POST">>, Database, Req, State) ->
  {ok, [{Body, _}], Req2} = cowboy_req:body_qs(Req),
  RequestedDocs = jsx:decode(Body, [return_maps]),
  Result = maps:fold(fun(DocId, RevIds, Acc) ->
    {ok, Missing, Possible} = barrel_local:revsdiff(Database, DocId, RevIds),
                         Acc#{DocId => #{<<"missing">> => Missing,
                                         <<"possible_ancestors">> => Possible}}
                     end,#{}, RequestedDocs),
  barrel_http_reply:doc(Result, Req2, State);


handle(_, _, Req, State) ->
  barrel_http_reply:code(405, Req, State).

terminate(_Reason, _Req, _State) ->
  ok.
