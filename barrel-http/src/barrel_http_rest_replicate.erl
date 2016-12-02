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
                   [#{ name => <<"name">>
                     , description => <<"Replication task name">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               },
       put => #{ summary => "Create a replication task"
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"name">>
                     , description => <<"Replication task name">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               },
       delete => #{ summary => "Delete a replication task"
                  , produces => ["application/json"]
                  , parameters =>
                      [#{ name => <<"name">>
                        , description => <<"Replication task name">>
                        , in => <<"path">>
                        , required => true
                        , type => <<"string">>}
                      ]
                  },
       patch => #{ summary => "Change replication tasks status and config"
                 , produces => ["application/json"]
                 , parameters =>
                     [#{ name => <<"name">>
                       , description => <<"Replication task name">>
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
                     [#{ name => <<"body">>
                       , description => <<"Parameters for the replication task">>
                       , in => <<"body">>
                       , required => true
                       , type => <<"json">>}
                    ]
               }
     },
  [trails:trail("/_replicate", ?MODULE, [], Post),
   trails:trail("/_replicate/:name", ?MODULE, [], GetPutDelete)].

-record(state, {method, name, body, source, target, persisted, started}).

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
  check_name(Req, State);
route(Req, #state{method= <<"GET">>}=State) ->
  check_name(Req, State);
route(Req, #state{method= <<"DELETE">>}=State) ->
  check_name(Req, State).

check_name(Req, State) ->
  {Name, Req2} = cowboy_req:binding(name, Req),
  case barrel:replication_info(Name) of
    {error, not_found} ->
      barrel_http_reply:error(404, "unknown replication task", Req2, State);
    _ ->
      check_body(Req2, State#state{name=Name})
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
  check_source_db_exist(Req, State);
route2(Req, #state{method= <<"PATCH">>}=State) ->
  patch_resource(Req, State).


check_source_db_exist(Req, #state{source=SourceUrl}=State) ->
  case barrel_http_lib:req(get, SourceUrl) of
    {200, _} ->
      check_target_db_exist(Req, State);
    _ ->
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
  {Name, Req2} = cowboy_req:binding(name, Req),
  case barrel:replication_info(Name) of
    {error, not_found} ->
      barrel_http_reply:error(404, "replication task not found", Req2, State);
    Infos ->
      #{metrics := Metrics} = Infos,
      barrel_http_reply:doc(Metrics, Req2, State)
  end.

create_resource(Req, #state{method=Method, source=SourceUrl, target=TargetUrl}=State) ->
  SourceConn = {barrel_httpc, SourceUrl},
  TargetConn = {barrel_httpc, TargetUrl},
  {ok, Name} = case Method of
                 <<"POST">> ->
                   barrel:start_replication(SourceConn, TargetConn, []);
                 <<"PUT">> ->
                   N = State#state.name,
                   barrel:start_replication(N, SourceConn, TargetConn, [])
               end,
  Doc = #{name => Name},
  barrel_http_reply:doc(Doc, Req, State).

delete_resource(Req, #state{name=Name}=State) ->
  ok = barrel:delete_replication(Name),
  barrel_http_reply:code(200, Req, State).


patch_resource(Req, #state{started=true}=State) ->
  barrel_http_reply:code(200, Req, State);

patch_resource(Req, #state{started=false}=State) ->
  barrel_http_reply:code(200, Req, State).

%% =============================================================================
%% Check posted JSON properties
%% =============================================================================

params() ->
  #{<<"POST">> =>
      #{<<"source">> => mandatory,
        <<"target">> => mandatory,
        <<"persited">> => {default, true}},
    <<"PUT">> =>
      #{<<"source">> => mandatory,
        <<"target">> => mandatory,
        <<"persited">> => {default, true}},
    <<"PACH">> =>
      #{<<"started">> => optional,
        <<"persisted">> => optional}
   }.

check_body_properties(OkFun, FailFun, Req, #state{method=Method, body=Body}=State) ->
  Map = params(),
  #{Method := Params} = Map,
  case check_params(Body, Params) of
    ok ->
      OkFun(Req, State);
    {error, {missing, Missing}} ->
      FailFun(<<"missing requirement property ", Missing/binary>>, Req, State);
    {error, {unknown, List}} ->
      Unknown = hd(List),
      FailFun(<<"unknown property ", Unknown/binary>>, Req, State)
  end.

check_params(Json, Params) ->
  Accepted = maps:to_list(Params),
  {Mandatories, Optionals} = lists:filter(fun({_,mandatory}) -> true;
                                             ({_,_}) -> false
                                          end, Accepted),
  Props = maps:keys(Json),
  case check_mandatories(Props, Mandatories) of
    {ok, Other} ->
      check_optionals(Other, Optionals);
    Error -> Error
  end.

check_mandatories(Props, Mandatories) ->
  case lists:dropwhile(fun(M) ->
                          maps:get(M, Props, undefined) =/= undefined
                       end, Mandatories) of
    true -> ok;
    Missings -> {error, {missing, hd(Missings)}}
  end.

check_optionals(Props, Optionals) ->
  case lists:filter(fun(P) ->
                        not lists:member(P, Optionals)
                    end, Props) of
    [] ->
      ok;
    Unknowns -> {error, {unknown, Unknowns}}
  end.

