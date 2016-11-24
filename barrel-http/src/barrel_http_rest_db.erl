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
                   , #{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               },
       put => #{ summary => "Create a new database"
                  , produces => ["application/json"]
                  , parameters =>
                      [#{ name => <<"dbid">>
                        , description => <<"Database ID">>
                        , in => <<"path">>
                        , required => true
                        , type => <<"string">>}
                      , #{ name => <<"store">>
                         , description => <<"Store ID">>
                         , in => <<"path">>
                         , required => true
                         , type => <<"string">>}
                      ]
                  }
     },
  [trails:trail("/:store/:dbid", ?MODULE, [], Metadata)].

-record(state, {method, store, databases, dbid}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  check_store(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

check_store(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {StoreBin, Req3} = cowboy_req:binding(store, Req2),
  {DbId, Req4} = cowboy_req:binding(dbid, Req3),
  Store = barrel_lib:to_atom(StoreBin),
  case barrel:database_names(Store) of
    {error, _} ->
      barrel_http_reply:code(400, Req, State);
    List ->
      route(Req4, State#state{method=Method, store=Store, databases=List, dbid=DbId})
  end.

route(Req, #state{method= <<"GET">>}=State) ->
  check_db_exists(Req, State);
route(Req, #state{method= <<"PUT">>}=State) ->
  create_resource(Req, State);
route(Req, #state{method= <<"POST">>, store=Store, dbid=Db}=State) ->
  barrel_http_rest_doc:post_put(post, Store, Db, undefined, Req, State);
route(Req, State) ->
  barrel_http_reply:code(405, Req, State).


create_resource(Req, #state{store=Store, dbid=Db}=State) ->
  case barrel:create_database(Store, Db) of
    {true, _} -> barrel_http_reply:json(201, #{ ok => true}, Req, State);
    {false, _} -> barrel_http_reply:json(412, #{ error => <<"db_exists">> }, Req, State);
    {error, Reason} -> barrel_http_reply:json(400, #{ error => Reason}, Req, State)
  end.


check_db_exists(Req, #state{databases=List, dbid=Db}=State) ->
  case lists:member(Db, List) of
    true ->
      get_resource(Req, State);
    false ->
      barrel_http_reply:code(404, Req, State)
  end.

get_resource(Req, #state{store=Store, dbid=Db}=State) ->
  {ok, Conn} = barrel:connect_database(Store, Db),
  {ok, Infos} = barrel:database_infos(Conn),
  [Id, Name, Store] = [maps:get(K, Infos) || K <- [id, name, store]],
  DbInfo = [{<<"id">>, Id},
            {<<"name">>, Name},
            {<<"store">>, Store}],
  barrel_http_reply:doc(DbInfo, Req, State).
