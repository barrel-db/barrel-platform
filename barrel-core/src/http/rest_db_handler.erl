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

-module(rest_db_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {DbId, Req3} = cowboy_req:binding(dbid, Req2),
    handle(Method, DbId, Req3, State).

handle(<<"GET">>, DbId, Req, State) ->
    {ok, Infos} = barrel_db:infos(DbId),
    [Id, Name, Store] = [maps:get(K, Infos) || K <- [id, name, store]],
    DbInfo = [{<<"id">>, Id},
              {<<"name">>, Name},
              {<<"store">>, Store}],
    Json = jsx:encode(DbInfo),
    {ok, Req2} = cowboy_req:reply(200, [
                                        {<<"content-type">>, <<"application/json">>}
                                       ], Json, Req),
    {ok, Req2, State};

handle(_, _, Req, State) ->
    {ok, Req2} = cowboy_req:reply(405, [], [], Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
