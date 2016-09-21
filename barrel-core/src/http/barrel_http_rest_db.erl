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

-module(barrel_http_rest_db).
-author("Bernard Notarianni").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Get information about a database."
               , produces => ["application/json"]
               , parameters => 
                   [#{ name => <<"dbid">>
                     , description => <<"Database ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               }
     },
  [trails:trail("/:dbid", ?MODULE, [], Metadata)].


init(_Type, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  handle(Method, DbId, Req3, State).

handle(<<"GET">>, DbId, Req, State) ->
  All = barrel:all_databases(),
  case lists:member(DbId, All) of
    false ->
      barrel_http_reply:code(404, Req, State);
    true ->
      db_info(DbId, Req, State)
  end;

handle(<<"POST">>, DbId, Req, State) ->
  barrel_http_rest_doc:post_put(post, DbId, undefined, Req, State);

handle(_, _, Req, State) ->
  barrel_http_reply:code(405, Req, State).

terminate(_Reason, _Req, _State) ->
  ok.

%% ----------

db_info(DbId, Req, State) ->
  {ok, Infos} = barrel_db:infos(DbId),
  [Id, Name, Store] = [maps:get(K, Infos) || K <- [id, name, store]],
  DbInfo = [{<<"id">>, Id},
            {<<"name">>, Name},
            {<<"store">>, Store}],
  barrel_http_reply:doc(DbInfo, Req, State).
