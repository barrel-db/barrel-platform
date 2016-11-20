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

-module(barrel_http_rest_doc).
-author("Bernard Notarianni").

-export([init/3]).
-export([rest_init/2]).

-export([allowed_methods/2,
         malformed_request/2,
         content_types_accepted/2, content_types_provided/2,
         resource_exists/2,
         delete_resource/2,
         allow_missing_post/2,
         is_conflict/2,
         from_json/2, to_json/2]).

-export([post_put/6]).

-export([trails/0]).

trails() ->
  GetPut =
    #{ get => #{ summary => "Get a document"
               , description => "Get a document."
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
  Post =
    #{post => #{ summary => "Add a new document."
               , produces => ["application/json"]
               , responses =>
                   #{ <<"200">> => #{ description => "Document added." }
                    }
               , parameters =>
                   [#{ name => <<"body">>
                     , description => <<"Document to be added">>
                     , in => <<"body">>
                     , required => true
                     , type => <<"json">>}
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
  [trails:trail("/:store/:dbid/", ?MODULE, [], Post),
   trails:trail("/:store/:dbid/:docid", ?MODULE, [], GetPut)].

-record(state, {method, store, dbid, docid, revid, doc, conn, options}).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) -> {ok, Req, #state{}}.


allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>,
             <<"POST">>, <<"GET">>, <<"PUT">>, <<"DELETE">>],
  {Methods, Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, from_json}],
   Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.


malformed_request(Req, S) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  malformed_request_store(Req2, S#state{store=Store}).

malformed_request_store(Req, #state{store=undefined}=S) ->
  {true, Req, S};
malformed_request_store(Req, S) ->
  {DbId, Req2} = cowboy_req:binding(dbid, Req),
  malformed_request_dbid(Req2, S#state{dbid=DbId}).

malformed_request_dbid(Req, #state{dbid=undefined}=S) ->
  {true, Req, S};
malformed_request_dbid(Req, #state{store=Store, dbid=DbId}=S) ->
  case barrel:connect_database(barrel_lib:to_atom(Store), DbId) of
    {ok, Conn} ->
      {Method, Req2} = cowboy_req:method(Req),
      {DocId, Req3} = cowboy_req:binding(docid, Req2),
      {RevId, Req4} = cowboy_req:qs_val(<<"rev">>, Req3),
      Options = case RevId of
                  undefined -> [];
                  _ -> [{rev, RevId}]
                end,
      malformed_request_revid(Req4, S#state{method=Method, docid=DocId, conn=Conn,
                                            revid=RevId, options=Options});
    {error, not_found} ->
      {true, Req, S}
  end.

malformed_request_revid(Req, #state{method= <<"DELETE">>, revid=undefined}=S) ->
  {true, Req, S};
malformed_request_revid(Res, S) ->
  {false, Res, S}.


resource_exists(Req, #state{method= <<"GET">>}=S) ->
  Conn = S#state.conn,
  DocId = S#state.docid,
  Options = S#state.options,
  case barrel:get(Conn, DocId, Options) of
    {error, not_found} -> {false, Req, S};
    {ok, Doc} -> {true, Req, S#state{doc=Doc}}
  end;

resource_exists(Req, #state{method= <<"DELETE">>}=S) ->
  {true, Req, S};

resource_exists(Req, State) ->
  {false, Req, State}.

allow_missing_post(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Json = jsx:decode(Body, [return_maps]),
  Conn = State#state.conn,
  {ok, DocId, RevId} = barrel:post(Conn, Json, []),
  Reply = #{<<"ok">> => true,
            <<"id">> => DocId,
            <<"rev">> => RevId},
  RespBody = jsx:encode(Reply),
  Req5 = cowboy_req:set_resp_body(RespBody, Req2),
  {true, Req5, State#state{docid=DocId}}.

is_conflict(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Json = jsx:decode(Body, [return_maps]),
  Conn = State#state.conn,
  DocId = State#state.docid,
  Method = State#state.method,
  {Result, Req4} = case Method of
                     <<"POST">> ->
                       {barrel:post(Conn, Json, []), Req2};
                     <<"PUT">> ->
                       {EditStr, Req3} = cowboy_req:qs_val(<<"edit">>, Req2),
                       Edit = ((EditStr =:= <<"true">>) orelse (EditStr =:= true)),
                       case Edit of
                         false ->
                           { barrel:put(Conn, DocId, Json, []), Req3 };
                         true ->
                           Doc = maps:get(<<"document">>, Json),
                           History = maps:get(<<"history">>, Json),
                           {barrel:put_rev(Conn, DocId, Doc, History, []), Req3}
                       end
                   end,
  case Result of
    %% {error, not_found} ->
    %%   barrel_http_reply:code(404, Req4, State);
    {error, {conflict, revision_conflict}} ->
      {true, Req4, State};
    {error, {conflict, doc_exists}} ->
      {true, Req4, State};
    {ok, _DocId, RevId} ->
      Reply = #{<<"ok">> => true,
                <<"id">> => DocId,
                <<"rev">> => RevId},
      RespBody = jsx:encode(Reply),
      Req5 = cowboy_req:set_resp_body(RespBody, Req2),
      {false, Req5, State}
  end.

delete_resource(Req, State) ->
  Conn = State#state.conn,
  DocId = State#state.docid,
  RevId = State#state.revid,
  {ok, DocId, RevId2} = barrel:delete(Conn, DocId, RevId, []),
  Reply = #{<<"ok">> => true,
            <<"id">> => DocId,
            <<"rev">> => RevId2},
  RespBody = jsx:encode(Reply),
  Req2 = cowboy_req:set_resp_body(RespBody, Req),
  {true, Req2, State}.

from_json(Req, State) ->
  {true, Req, State}.

to_json(Req, State) ->
  Doc = State#state.doc,
  Json = jsx:encode(Doc),
  {Json, Req, State}.


%% ------
%% Legacy

post_put(Method, Store, DbId, DocIdAsBin, Req, State) ->
  case barrel:connect_database(barrel_lib:to_atom(Store), DbId) of
    {ok, Conn} ->
      post_put(Method, Conn, DocIdAsBin, Req, State);
    {error, not_found} ->
      barrel_http_reply:code(404, Req, State)
  end.
post_put(Method, Conn, DocIdAsBin, Req, State) ->
  {ok, [{Body, _}], Req2} = cowboy_req:body_qs(Req),
  Json = jsx:decode(Body, [return_maps]),
  {Result, Req4} = case Method of
        post ->
          {barrel:post(Conn, Json, []), Req2};
        put ->
          {EditStr, Req3} = cowboy_req:qs_val(<<"edit">>, Req2),
          Edit = ((EditStr =:= <<"true">>) orelse (EditStr =:= true)),
          case Edit of
            false ->
              { barrel:put(Conn, DocIdAsBin, Json, []), Req3 };
            true ->
              Doc = maps:get(<<"document">>, Json),
              History = maps:get(<<"history">>, Json),
              {barrel:put_rev(Conn, DocIdAsBin, Doc, History, []), Req3}
          end
      end,
  case Result of
    {error, not_found} ->
      barrel_http_reply:code(404, Req4, State);
    {error, {conflict, revision_conflict}} ->
      barrel_http_reply:code(409, Req4, State);
    {error, {conflict, doc_exists}} ->
      barrel_http_reply:code(409, Req4, State);
    {ok, DocId, RevId} ->
      Reply = #{<<"ok">> => true,
                <<"id">> => DocId,
                <<"rev">> => RevId},
      barrel_http_reply:doc(Reply, Req4, State)
  end.
