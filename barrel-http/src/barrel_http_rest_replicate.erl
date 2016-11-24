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
  Get =
    #{ get => #{ summary => "Get metics about a replication tasks"
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"repid">>
                     , description => <<"Replication task ID">>
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
   trails:trail("/_replicate/:repid", ?MODULE, [], Get)].

-record(state, {method, body, source, target}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.



route(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
route(Req, #state{method= <<"POST">>}=State) ->
  check_body(Req, State).

check_body(Req, State) ->
  check_json(Req, State).


check_json(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  try jsx:decode(Body, [return_maps]) of
      Json ->
      check_source(Req, State#state{body=Json})
  catch
    _:_ ->
      barrel_http_reply:error(400, "json malformed", Req2, State)
  end.


check_source(Req, State) ->
  Body = State#state.body,
  case maps:get(<<"source">>, Body, undefined) of
    undefined ->
      barrel_http_reply:error(400, "source property missing", Req, State);
    Source ->
      check_target(Req, State#state{source=Source})
  end.

check_target(Req, State) ->
  Body = State#state.body,
  case maps:get(<<"target">>, Body, undefined) of
    undefined ->
      barrel_http_reply:error(400, "target property missing", Req, State);
    Source ->
      check_db_exists(Req, State#state{target=Source})
  end.


check_db_exists(Req, State) ->
  check_source_db_exist(Req, State).

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
  {RepId, Req2} = cowboy_req:binding(repid, Req),
  case barrel:replication_info(RepId) of
    {error, not_found} ->
      barrel_http_reply:error(404, "replication task not found", Req2, State);
    Infos ->
      #{metrics := Metrics} = Infos,
      barrel_http_reply:doc(Metrics, Req2, State)
  end.

create_resource(Req, #state{source=SourceUrl, target=TargetUrl}=State) ->
  SourceConn = {barrel_httpc, SourceUrl},
  TargetConn = {barrel_httpc, TargetUrl},
  {ok, RepId} = barrel:start_replication(SourceConn, TargetConn, []),
  Doc = #{repid => RepId},
  barrel_http_reply:doc(Doc, Req, State).
