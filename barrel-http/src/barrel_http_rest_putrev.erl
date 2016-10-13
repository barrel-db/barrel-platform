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

-module(barrel_http_rest_putrev).
-author("Bernard Notarianni").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ put => #{ summary => "Insert a specific revision for a document"
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"body">>
                     , description => <<"Document and history as JSON">>
                     , in => <<"body">>
                     , required => true
                     , type => <<"json">>}
                   ]
               }
     },
  [trails:trail("/:dbid/:docid/_revs", ?MODULE, [], Metadata)].


init(_Type, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  {DocId, Req4} = cowboy_req:binding(docid, Req3),
  handle(Method, DbId, DocId, Req4, State).

handle(<<"PUT">>, DbId, DocId, Req, State) ->
  {ok, [{Body, _}], Req2} = cowboy_req:body_qs(Req),
  BodyJson = jsx:decode(Body, [return_maps]),
  Doc = maps:get(<<"document">>, BodyJson),
  History = maps:get(<<"history">>, BodyJson),
  Conn = barrel_http_conn:peer(DbId),
  {ok, DocId, RevId} = barrel_db:put_rev(Conn, DocId, Doc, History, []),
  Result = #{<<"ok">> => true,
             <<"_id">> => DocId,
             <<"_rev">> => RevId},
  barrel_http_reply:doc(Result, Req2, State);


handle(_, _, _, Req, State) ->
  barrel_http_reply:code(405, Req, State).

terminate(_Reason, _Req, _State) ->
  ok.
