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


%% API
-export([init/3]).
-export([rest_init/2]).

-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([to_json/2]).
-export([from_json/2]).

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


-record(state, {conn, docid, doc}).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) -> {ok, Req, #state{}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"GET">> , <<"PUT">>, <<"DELETE">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, from_json}],
   Req, State}.

resource_exists(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  {DocId, Req4} = cowboy_req:binding(docid, Req3),
  {ok, Conn} = barrel_http_conn:peer(Store, DbId),
  {ok, Body, _} = cowboy_req:body(Req),
  {Method, _} = cowboy_req:method(Req),
  ct:print("resource exists method-~p ~p ~p ~p body=~p",[Method, Store, DbId, DocId, Body]),
  ct:print("  req=~p",[Req]),
  State2 = State#state{docid=DocId, conn=Conn},
  case barrel_db:read_system_doc(Conn, DocId) of
    {ok, Doc} ->
      {true, Req4, State2#state{doc=Doc}};
    {error, not_found} ->
      {false, Req4, State2}
  end.


to_json(Req, #state{doc=Doc}=State) ->
  Json = jsx:encode(Doc),
  {Json, Req, State}.

from_json(Req, #state{conn=Conn, docid=DocId}=State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Doc = jsx:decode(Body, [return_maps]),
  ct:print("write system doc ~p",[Doc]),
  ok = barrel_db:write_system_doc(Conn, DocId, Doc),
  {true, Req2, State}.

delete_resource(Req, #state{conn=Conn, docid=DocId}=State) ->
  ok = barrel_db:delete_system_doc(Conn, DocId),
  {true, Req, State}.


