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
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {IdAsBin, Req3} = cowboy_req:binding(dbid, Req2),
    handle(Method, IdAsBin, Req3, State).

handle(<<"GET">>, _IdAsBin, Req, State) ->
    {_Since, Req2} = cowboy_req:qs_val(<<"since">>, Req),
    Results =  [#{<<"changes">> => [#{<<"rev">> => <<"2-7051cbe5c8faecd085a3fa619e6e6337">>}],
                  <<"id">> => <<"6478c2ae800dfc387396d14e1fc39626">>,
                  <<"seq">> => 6}
               ],
    LastSeq = 11,
    Result = #{<<"last_seq">> => LastSeq, <<"results">> => Results},
    http_reply:doc(Result, Req2, State);


handle(_, _, Req, State) ->
    http_reply:code(405, Req, State).

terminate(_Reason, _Req, _State) ->
    ok.
