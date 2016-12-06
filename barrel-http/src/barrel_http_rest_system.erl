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

-module(barrel_http_rest_system).
-author("Bernard Notarianni").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  GetPutDelete =
    #{ get => #{ summary => "Get a system document"
               , description => "Get a document from the system storage"
               , produces => ["application/json"]
               , responses =>
                   #{ <<"200">> => #{ description => "Document found." }
                    , <<"404">> => #{ description => "Document not found." }
                    }
               , parameters =>
                   [#{ name => <<"docid">>
                     , description => <<"Document ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ,#{ name => <<"dbid">>
                     , description => <<"Database ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ,#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               }
     , delete => #{ summary => "Delete a system document"
                  , description => "Delete a document from the system storage"
                  , produces => ["application/json"]
                  , parameters =>
                      [#{ name => <<"docid">>
                        , description => <<"Document ID">>
                        , in => <<"path">>
                        , required => true
                        , type => <<"string">>}
                      ,#{ name => <<"dbid">>
                        , description => <<"Database ID">>
                        , in => <<"path">>
                        , required => true
                        , type => <<"string">>}
                      ,#{ name => <<"store">>
                        , description => <<"Store ID">>
                        , in => <<"path">>
                        , required => true
                        , type => <<"string">>}
                      ]
                  }
     , put => #{ summary => "Add/update a document."
               , produces => ["application/json"]
               , responses =>
                   #{ <<"200">> => #{ description => "Document updated." }
                    }
               , parameters =>
                   [#{ name => <<"body">>
                     , description => <<"Document to be added">>
                     , in => <<"body">>
                     , required => true
                     , type => <<"application/json">>}
                   ,#{ name => <<"docid">>
                     , description => <<"Document ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ,#{ name => <<"dbid">>
                     , description => <<"Database ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ,#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               }
     },
  [trails:trail("/:store/:dbid/_system/:docid", ?MODULE, [], GetPutDelete)].


-record(state, {conn, method, store, dbid, docid, doc}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  check_store_db(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.


check_store_db(Req, State) ->
    {Store, Req2} = cowboy_req:binding(store, Req),
    {DbId, Req3} = cowboy_req:binding(dbid, Req2),
    case barrel:connect_database(barrel_lib:to_atom(Store), DbId) of
      {error, {unknown_store, _}} ->
        barrel_http_reply:error(400, <<"store not found: ", Store/binary>>, Req3, State);
      {error, not_found} ->
        barrel_http_reply:error(400, <<"database not found: ", DbId/binary>>, Req3, State);
      {ok, Conn} ->
        {DocId, Req2} = cowboy_req:binding(docid, Req),
        State2 = State#state{store=Store, dbid=DbId, docid=DocId, conn=Conn},
        route(Req3, State2)
    end.


route(Req, #state{method= <<"PUT">>}=State) ->
  create_resource(Req, State);
route(Req, #state{method= <<"GET">>}=State) ->
  check_resource_exists(Req, State);
route(Req, #state{method= <<"DELETE">>}=State) ->
  check_resource_exists(Req, State);
route(Req, State) ->
  barrel_http_reply:code(405, Req, State).

check_resource_exists(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  {DocId, Req4} = cowboy_req:binding(docid, Req3),
  {ok, Conn} = barrel:connect_database(barrel_lib:to_atom(Store), DbId),
  State2 = State#state{docid=DocId, conn=Conn},
  case barrel_db:read_system_doc(Conn, DocId) of
    {ok, Doc} ->
      route2(Req4, State2#state{doc=Doc});
    {error, not_found} ->
      barrel_http_reply:error(404, Req, State)
  end.

route2(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
route2(Req, #state{method= <<"DELETE">>}=State) ->
  delete_resource(Req, State).

get_resource(Req, #state{doc=Doc}=State) ->
  barrel_http_reply:doc(Doc, Req, State).

create_resource(Req, #state{conn=Conn, docid=DocId}=State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Doc = jsx:decode(Body, [return_maps]),
  ok = barrel_db:write_system_doc(Conn, DocId, Doc),
  barrel_http_reply:doc(#{ok => true}, Req2, State).

delete_resource(Req, #state{conn=Conn, docid=DocId}=State) ->
  ok = barrel_db:delete_system_doc(Conn, DocId),
  barrel_http_reply:doc(#{ok => true}, Req, State).
