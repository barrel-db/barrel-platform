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

-module(barrel_http_rest_replicate).
-author("Bernard Notarianni").


%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  GetPutDelete =
    #{ get => #{ summary => "Get metics about a replication tasks"
               , produces => ["application/json"]
               , parameters =>
                   [#{ repid => <<"repid">>
                     , description => <<"Replication task repid">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               },
       put => #{ summary => "Create a replication task"
               , produces => ["application/json"]
               , parameters =>
                   [#{ repid => <<"body">>
                     , description => <<"Parameters for the replication task">>
                     , in => <<"body">>
                     , required => true
                     , type => <<"json">>},
                    #{ repid => <<"repid">>
                     , description => <<"Replication task repid">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               },
       delete => #{ summary => "Delete a replication task"
                  , produces => ["application/json"]
                  , parameters =>
                      [#{ repid => <<"repid">>
                        , description => <<"Replication task repid">>
                        , in => <<"path">>
                        , required => true
                        , type => <<"string">>}
                      ]
                  }
     },
  Post =
    #{ post => #{ summary => "Create a replication tasks"
                , produces => ["application/json"]
                , parameters =>
                     [#{ repid => <<"body">>
                       , description => <<"Parameters for the replication task">>
                       , in => <<"body">>
                       , required => true
                       , type => <<"json">>}
                    ]
               }
     },
  [trails:trail("/replicate", ?MODULE, [], Post),
   trails:trail("/replicate/:repid", ?MODULE, [], GetPutDelete)].

-record(state, {method, repid, body, source, target, persisted, started}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.



route(Req, #state{method= <<"POST">>}=State) ->
  check_body(Req, State);
route(Req, #state{method= <<"PUT">>}=State) ->
  check_body(Req, State);
route(Req, #state{method= <<"GET">>}=State) ->
  check_repid(Req, State);
route(Req, #state{method= <<"DELETE">>}=State) ->
  check_repid(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).

check_repid(Req, State) ->
  {Repid, Req2} = cowboy_req:binding(repid, Req),
  case barrel_replicate:replication_info(Repid) of
    {error, not_found} ->
      barrel_http_reply:error(404, <<"unknown replication task: ", Repid/binary>>, Req2, State);
    _ ->
      check_body(Req2, State#state{repid=Repid})
  end.

check_body(Req, #state{method= <<"POST">>}=State) ->
  check_json_is_valid(Req, State);
check_body(Req, #state{method= <<"PUT">>}=State) ->
  check_json_is_valid(Req, State);
check_body(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
check_body(Req, #state{method= <<"DELETE">>}=State) ->
  delete_resource(Req, State).

check_json_is_valid(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  try jsx:decode(Body, [return_maps]) of
      Json ->
      check_json_properties(Req, State#state{body=Json})
  catch
    _:_ ->
      barrel_http_reply:error(400, "json malformed", Req2, State)
  end.

check_json_properties(Req, State) ->
  OkFun = fun read_json_properties/2,
  FailFun = fun(Message, R, S) ->
                barrel_http_reply:error(400, Message, R, S)
            end,
  check_body_properties(OkFun, FailFun, Req, State).


read_json_properties(Req, State) ->
  Body = State#state.body,
  State2 = State#state{source = maps:get(<<"source">>, Body, undefined),
                       target = maps:get(<<"target">>, Body, undefined),
                       started = maps:get(<<"started">>, Body, undefined),
                       persisted = maps:get(<<"persisted">>, Body, undefined)},
  route2(Req, State2).

route2(Req, #state{method= <<"POST">>}=State) ->
  check_source_db_exist(Req, State);
route2(Req, #state{method= <<"PUT">>}=State) ->
  check_source_db_exist(Req, State).


check_source_db_exist(Req, #state{source=SourceUrl}=State) ->
  case barrel_http_lib:req(get, SourceUrl) of
    {200, _} ->
      check_target_db_exist(Req, State);
    _Else ->
      barrel_http_reply:error(400, "source database not found", Req, State)
  end.

check_target_db_exist(Req, #state{target=TargetUrl}=State) ->
  case barrel_http_lib:req(get, TargetUrl) of
    {200, _} ->
      create_resource(Req, State);
    _ ->
      barrel_http_reply:error(400, "target database not found", Req, State)
  end.



get_resource(Req, State) ->
  {Repid, Req2} = cowboy_req:binding(repid, Req),
  case barrel_replicate:replication_info(Repid) of
    {error, not_found} ->
      barrel_http_reply:error(404, "replication task not found", Req2, State);
    Infos ->
      #{metrics := Metrics} = Infos,
      barrel_http_reply:doc(Metrics, Req2, State)
  end.

create_resource(Req, #state{source=SourceUrl, target=TargetUrl}=State) ->
  {ReqRepid, Req2} = cowboy_req:binding(repid, Req),
  SourceConn = {barrel_httpc, SourceUrl},
  TargetConn = {barrel_httpc, TargetUrl},
  {ok, Rep} = case ReqRepid of
                undefined ->
                  RepConfig = #{<<"source">> => SourceConn,
                                <<"target">> => TargetConn},
                  barrel_replicate:start_replication(RepConfig, []);
                _ ->
                  RepConfig = #{<<"replication_id">> => ReqRepid,
                                <<"source">> => SourceConn,
                                <<"target">> => TargetConn},
                  barrel_replicate:start_replication(RepConfig, [])
              end,
  #{<<"replication_id">> := RepId} = Rep,
  Doc = #{<<"replication_id">> => RepId},
  barrel_http_reply:doc(Doc, Req2, State).

delete_resource(Req, #state{repid=Repid}=State) ->
  ok = barrel_replicate:delete_replication(Repid),
  barrel_http_reply:code(200, Req, State).

%% =============================================================================
%% Check posted JSON properties
%% =============================================================================

params() ->
  #{<<"POST">> =>
      #{<<"source">> => mandatory,
        <<"target">> => mandatory,
        <<"persisted">> => optional},
    <<"PUT">> =>
      #{<<"source">> => mandatory,
        <<"target">> => mandatory,
        <<"persisted">> => optional}
   }.

check_body_properties(OkFun, FailFun, Req, #state{method=Method, body=Body}=State) ->
  Map = params(),
  #{Method := Params} = Map,
  case check_params(Body, Params) of
    {ok, _} ->
      OkFun(Req, State);
    {error, {missing, Missing}} ->
      FailFun(<<"missing requirement property ", Missing/binary>>, Req, State);
    {error, {unknown, List}} ->
      {Unknown,_} = hd(List),
      FailFun(<<"unknown property ", Unknown/binary>>, Req, State)
  end.

check_params(Json, Params) ->
  JsonKeys = maps:keys(Json),
  Props = maps:from_list([{K, unknown} || K <- JsonKeys]),
  Accepted = maps:to_list(Params),
  {Mandatories, Optionals} = lists:partition(fun({_,mandatory}) -> true;
                                                ({_,_}) -> false
                                             end, Accepted),
  case check_mandatories(Mandatories, Props) of
    {ok, Props2} ->
      check_optionals(Optionals, Props2);
    {error, E} ->
      {error, E}
  end.

check_mandatories([], Props) ->
  {ok, Props};
check_mandatories([{M,mandatory}|Tail], Props) ->
  case maps:is_key(M, Props) of
    true ->
      check_mandatories(Tail, Props#{M => mandatory});
    false ->
      {error, {missing, M}}
  end.

check_optionals([], Props) ->
  Unknown = lists:filter(fun({_,unknown}) -> true;
                            (_) -> false
end, maps:to_list(Props)),
case Unknown of
    [] ->
      {ok, Props};
    L ->
      {error, {unknown, L}}
  end;
check_optionals([{O, optional}|Tail], Props) ->
  case maps:is_key(O, Props) of
    true ->
      check_optionals(Tail, Props#{O => optional});
    false ->
      check_optionals(Tail, Props)
  end.

