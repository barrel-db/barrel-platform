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

terminate(_Reason, _Req, #state{database=Db, method=Method, start_time=StartTime}) ->
  Key = [<<"rest">>, Db, Method, <<"duration">>],
  barrel_metrics:duration_since(Key, StartTime),
  ok;
terminate(_, _, _) ->
  ok.


check_params(Req, State) ->
  Params = cowboy_req:parse_qs(Req),
  case parse_params(Params, State) of
    {error, {unknown_param, Unknown}} ->
      barrel_http_reply:error(400, <<"unknown query parameter: ", Unknown/binary>>, Req, State);
    {ok, S2} ->
      {ok, Body, Req2} = cowboy_req:read_body(Req),
      Options = State#state.options,
      Opts = case S2#state.history of
                true -> [{history, true}|Options];
                _ -> Options
              end,
      route(Req2, S2#state{body=Body, options=Opts})
  end.

parse_params([], State) ->
  {ok, State};
parse_params([{<<"edit">>, Edit}|Tail], State) ->
  parse_params(Tail, State#state{edit=Edit});
parse_params([{<<"history">>, <<"true">>}|Tail], State) ->
  parse_params(Tail, State#state{history=true});
parse_params([{<<"async">>, <<"true">>}|Tail], State) ->
  parse_params(Tail, State#state{async=true});
parse_params([{Param, __Value}|_], _State) ->
  io:format("unknown param ~p~n", [Param]),
  {error, {unknown_param, Param}}.


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
  DocId = cowboy_req:binding(docid, Req),
  case Json of
    #{ <<"id">> := DocId} ->
      route2(Req, State);
    #{ <<"id">> := _ } ->
      barrel_http_reply:error(400, <<"id in document differs from the path">>, Req, State);
    _ ->
      barrel_http_reply:error(400, <<"missing property id in document">>, Req, State)
  end.


check_resource_exists(Req, #state{method= <<"DELETE">>}=S) ->
  check_resource_exists2(Req, S);
check_resource_exists(Req, #state{method= <<"GET">>}=S) ->
  check_resource_exists2(Req, S);
check_resource_exists(Req, S) ->
  route2(Req, S).

check_resource_exists2(Req, S) ->
  #state{ database=Database, docid=DocId, options=Options } = S,
  case barrel_local:get(Database, DocId, Options) of
    {error, not_found} ->
      barrel_http_reply:error(404, Req, S);
    {ok, Doc, Meta} ->
      route2(Req, S#state{doc=Doc, meta=Meta})
  end.


route2(Req, #state{method= <<"POST">>}=State) ->
  create_resource(Req, State);
route2(Req, #state{method= <<"PUT">>}=State) ->
  create_resource(Req, State);
route2(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
route2(Req, #state{method= <<"DELETE">>}=State) ->
  delete_resource(Req, State).


create_resource(Req, State) ->
  #state{ database=Database, body=Json, method=Method, options=Options} = State,
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
                           {barrel_local:put(Database, Json, [{async, Async}|Options]), Req };
                         true ->
                           Doc = maps:get(<<"document">>, Json),
                           History = maps:get(<<"history">>, Json),
                           Deleted = maps:get(<<"deleted">>, Json, false),
                           {barrel_local:put_rev(Database, Doc, History, Deleted, [{async, Async}|Options]), Req }
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
      Req5 = cowboy_req:set_resp_header(<<"etag">>, RevId, Req4),
      barrel_http_reply:doc(201, Json#{<<"id">> => CreatedDocId}, Req5, State);
    ok ->
      barrel_http_reply:doc(201, #{<<"ok">> => true}, Req4, State)

  end.

delete_resource(Req, State) ->
  #{async := AsyncStr}
    = cowboy_req:match_qs([{async, [], undefined}], Req),
  Async = ((AsyncStr =:= <<"true">>) orelse (AsyncStr =:= true)),

  #state{ database=Database, docid=DocId, options=Options} = State,
  Result = barrel_local:delete(Database, DocId, [{async, Async}|Options]),
  case Result of
    {ok, DocId, RevId2} ->
      Reply = #{<<"ok">> => true,
                <<"id">> => DocId,
                <<"rev">> => RevId2},
      barrel_http_reply:doc(Reply, Req, State);
    {error, {conflict, revision_conflict}} ->
      barrel_http_reply:error(409, <<"revision conflict">>, Req, State)
  end.


get_resource(Req, State) ->
  Doc = State#state.doc,
  Meta = State#state.meta,
  RevId = maps:get(<<"rev">>, Meta),
  Req2 = case State#state.history of
           true ->
             RevIds = barrel_doc:parse_revisions(Meta),
             Joined = barrel_lib:binary_join(RevIds, <<",">>),
             cowboy_req:set_resp_header(<<"x-barrel-revisions-id">>, Joined, Req);
           _ -> Req
         end,
  Req3 = cowboy_req:set_resp_header(<<"ETag">>, RevId, Req2),
  Req4 = case maps:get(<<"deleted">>, Meta, false) of
           true ->
             cowboy_req:set_resp_header(<<"x-barrel-deleted">>, <<"true">>, Req3);
           _ -> Req3
         end,
  barrel_http_reply:doc(Doc, Req4, State).
