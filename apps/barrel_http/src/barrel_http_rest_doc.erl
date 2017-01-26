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

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

-export([handle_post/1]).

trails() ->
  GetPutDel =
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
                   ,#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               }
     , delete => #{ summary => "Delete a document."
                  , produces => ["application/json"]
                  , responses =>
                      #{ <<"200">> => #{ description => "Document deleted." }
                       }
                  , parameters =>
                      [#{ name => <<"rev">>
                         , description => <<"Last document revision">>
                         , in => <<"query">>
                         , required => true
                         , type => <<"string">>}
                      , #{ name => <<"docid">>
                        , description => <<"Document ID">>
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
                   ,#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               }
     },
  [trails:trail("/:store/", ?MODULE, [], Post),
   trails:trail("/:store/:docid", ?MODULE, [], GetPutDel)].

-record(state, {method, store, docid, revid, edit, history, body, doc, conn, options}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle_post(Req) ->
  handle(Req, #state{}).

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  check_params(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

check_params(Req, State) ->
  {Params, Req2} = cowboy_req:qs_vals(Req),
  case parse_params(Params, State) of
    {error, {unknown_param, Unknown}} ->
      barrel_http_reply:error(400, <<"unknown query parameter: ", Unknown/binary>>, Req2, State);
    {ok, S2} ->
      {Method, Req3} = cowboy_req:method(Req2),
      {ok, Body, Req4} = cowboy_req:body(Req3),
      check_request_revid(Req4, S2#state{method=Method, body=Body})
  end.

parse_params([], State) ->
  {ok, State};
parse_params([{<<"rev">>, RevId}|Tail], State) ->
  parse_params(Tail, State#state{revid=RevId});
parse_params([{<<"edit">>, Edit}|Tail], State) ->
  parse_params(Tail, State#state{edit=Edit});
parse_params([{<<"history">>, <<"true">>}|Tail], State) ->
  parse_params(Tail, State#state{history=true});
parse_params([{Param, __Value}|_], _State) ->
  {error, {unknown_param, Param}}.

check_request_revid(Req, #state{method= <<"DELETE">>, revid=undefined}=S) ->
  barrel_http_reply:error(400, <<"mising rev parameter">>, Req, S);
check_request_revid(Req, S) ->
  Body = S#state.body,
  check_store_db(Req, S#state{body=Body}).

check_store_db(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  case barrel_http_lib:has_store(Store) of
    false ->
      barrel_http_reply:error(400, <<"store not found: ", Store/binary>>, Req2, State);
    true ->
      {DocId, Req3} = cowboy_req:binding(docid, Req2),
      RevId = State#state.revid,
      Opts1 = case RevId of
                undefined -> [];
                _ -> [{rev, RevId}]
              end,
      Opts2 = case State#state.history of
                true -> [{history, true}|Opts1];
                _ -> Opts1
              end,
      State2 =  State#state{
        store=Store,
        docid=DocId,
        revid=RevId,
        options=Opts2
      },
      route(Req3, State2)
  end.

route(Req, #state{method= <<"POST">>}=State) ->
  check_body(Req, State);
route(Req, #state{method= <<"PUT">>}=State) ->
  check_body(Req, State);
route(Req, #state{method= <<"GET">>}=State) ->
  check_resource_exists(Req, State);
route(Req, #state{method= <<"DELETE">>}=State) ->
  check_resource_exists(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).


check_body(Req, #state{body= <<>>}=S) ->
  barrel_http_reply:error(400, <<"empty body">>, Req, S);
check_body(Req, #state{body=Body}=S) ->
  try jsx:decode(Body, [return_maps]) of
      Json ->
      check_json_properties(Req, S#state{body=Json})
  catch
    _:_ ->
      barrel_http_reply:error(400, <<"malformed json document">>, Req, S)
  end.

check_json_properties(Req, #state{method= <<"PUT">>, edit=undefined}=State) ->
  check_id_property(Req, State);
check_json_properties(Req, State) ->
  check_resource_exists(Req, State).


check_id_property(Req, #state{body=Json}=State) ->
  {DocId, Req2} = cowboy_req:binding(docid, Req),
  case Json of
    #{ <<"id">> := DocId} ->
      route2(Req2, State);
    #{ <<"id">> := _ } ->
      barrel_http_reply:error(400, <<"id in document differs from the path">>, Req2, State);
    _ ->
      barrel_http_reply:error(400, <<"missing property id in document">>, Req2, State)
  end.


check_resource_exists(Req, #state{method= <<"GET">>}=S) ->
  #state{ store=Store, docid=DocId, options=Options } = S,
  case barrel:get(Store, DocId, Options) of
    {error, not_found} ->
      barrel_http_reply:error(404, Req, S);
    {ok, Doc} ->
      route2(Req, S#state{doc=Doc})
  end;
check_resource_exists(Req, State) ->
  route2(Req, State).


route2(Req, #state{method= <<"POST">>}=State) ->
  create_resource(Req, State);
route2(Req, #state{method= <<"PUT">>}=State) ->
  create_resource(Req, State);
route2(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
route2(Req, #state{method= <<"DELETE">>}=State) ->
  delete_resource(Req, State).


create_resource(Req, State) ->
  #state{ store=Store, body=Json, method=Method} = State,
  {Result, Req4} = case Method of
                     <<"POST">> ->
                       {barrel:post(Store, Json, []), Req};
                     <<"PUT">> ->
                       {EditStr, Req3} = cowboy_req:qs_val(<<"edit">>, Req),
                       Edit = ((EditStr =:= <<"true">>) orelse (EditStr =:= true)),
                       case Edit of
                         false ->
                           { barrel:put(Store, Json, []), Req3 };
                         true ->
                           Doc = maps:get(<<"document">>, Json),
                           History = maps:get(<<"history">>, Json),
                           { barrel:put_rev(Store, Doc, History, []), Req3 }
                       end
                   end,
  case Result of
    {error, not_found} ->
      barrel_http_reply:error(404, Req4, State);
    {error, {conflict, revision_conflict}} ->
      barrel_http_reply:error(409, <<"revision conflict">>, Req4, State);
    {error, {conflict, doc_exists}} ->
      barrel_http_reply:error(409, <<"document exists">>, Req4, State);
    {ok, CreatedDocId, RevId} ->
      Reply = #{<<"ok">> => true,
                <<"id">> => CreatedDocId,
                <<"rev">> => RevId},
      barrel_http_reply:doc(201, Reply, Req4, State)
  end.

delete_resource(Req, State) ->
  #state{ store=Store, docid=DocId, revid=RevId} = State,
  {ok, DocId, RevId2} = barrel:delete(Store, DocId, RevId, []),
  Reply = #{<<"ok">> => true,
            <<"id">> => DocId,
            <<"rev">> => RevId2},
  barrel_http_reply:doc(Reply, Req, State).


get_resource(Req, State) ->
  Doc = State#state.doc,
  barrel_http_reply:doc(Doc, Req, State).
