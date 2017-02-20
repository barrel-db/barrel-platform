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

-module(barrel_http_rest_docs_id).
-author("Bernard Notarianni").

%% API
-export([init/2]).
-export([handle/2]).
-export([terminate/3]).

-export([handle_post/1]).

-include("barrel_http_rest_docs.hrl").

init(Req, State) ->
  {ok, Req, State}.

handle_post(Req) ->
  handle(Req, #state{}).

handle(Req, State) ->
  check_params(Req, State).

terminate(_Reason, _Req, _State) ->
  ok.


check_params(Req, State) ->
  Params = cowboy_req:parse_qs(Req),
  case parse_params(Params, State) of
    {error, {unknown_param, Unknown}} ->
      barrel_http_reply:error(400, <<"unknown query parameter: ", Unknown/binary>>, Req, State);
    {ok, S2} ->
      {ok, Body, Req2} = cowboy_req:read_body(Req),

      Opts1 = case S2#state.revid of
                undefined -> [];
                RevId -> [{rev, RevId}]
              end,
      Opts2 = case S2#state.history of
                true -> [{history, true}|Opts1];
                _ -> Opts1
              end,
      route(Req2, S2#state{body=Body, options=Opts2})
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



route(Req, #state{method= <<"POST">>}=State) ->
  check_body(Req, State);
route(Req, #state{method= <<"PUT">>}=State) ->
  check_body(Req, State);
route(Req, #state{method= <<"GET">>}=State) ->
  check_resource_exists(Req, State);
route(Req, #state{method= <<"DELETE">>}=State) ->
  check_request_revid(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).


check_request_revid(Req, #state{method= <<"DELETE">>, revid=undefined}=S) ->
  barrel_http_reply:error(400, <<"mising rev parameter">>, Req, S);
check_request_revid(Req, S) ->
  Body = S#state.body,
  check_resource_exists(Req, S#state{body=Body}).


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
  DocId = cowboy_req:binding(docid, Req),
  case Json of
    #{ <<"id">> := DocId} ->
      route2(Req, State);
    #{ <<"id">> := _ } ->
      barrel_http_reply:error(400, <<"id in document differs from the path">>, Req, State);
    _ ->
      barrel_http_reply:error(400, <<"missing property id in document">>, Req, State)
  end.


check_resource_exists(Req, #state{method= <<"GET">>}=S) ->
  #state{ database=Database, docid=DocId, options=Options } = S,
  case barrel_local:get(Database, DocId, Options) of
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
  #state{ database=Database, body=Json, method=Method} = State,
  #{async := AsyncStr}
    = cowboy_req:match_qs([{async, [], undefined}], Req),
  Async = ((AsyncStr =:= <<"true">>) orelse (AsyncStr =:= true)),
  {Result, Req4} = case Method of
                     <<"POST">> ->
                       {barrel_local:post(Database, Json, [{async, Async}]), Req};
                     <<"PUT">> ->
                       #{edit := EditStr}
                         = cowboy_req:match_qs([{edit, [], undefined}], Req),
                       Edit = ((EditStr =:= <<"true">>) orelse (EditStr =:= true)),
                       case Edit of
                         false ->
                           {barrel_local:put(Database, Json, [{async, Async}]), Req };
                         true ->
                           Doc = maps:get(<<"document">>, Json),
                           History = maps:get(<<"history">>, Json),
                           {barrel_local:put_rev(Database, Doc, History, [{async, Async}]), Req }
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
  #{async := AsyncStr}
    = cowboy_req:match_qs([{async, [], undefined}], Req),
  Async = ((AsyncStr =:= <<"true">>) orelse (AsyncStr =:= true)),

  #state{ database=Database, docid=DocId, revid=RevId} = State,
  {ok, DocId, RevId2} = barrel_local:delete(Database, DocId, RevId, [{async, Async}]),
  Reply = #{<<"ok">> => true,
            <<"id">> => DocId,
            <<"rev">> => RevId2},
  barrel_http_reply:doc(Reply, Req, State).


get_resource(Req, State) ->
  Doc = State#state.doc,
  barrel_http_reply:doc(Doc, Req, State).
