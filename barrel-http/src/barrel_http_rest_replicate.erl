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
-export([rest_init/2]).

-export([allowed_methods/2,
         content_types_accepted/2, content_types_provided/2,
         from_json/2, to_json/2]).

-export([trails/0]).

trails() ->
  Get =
    #{ get => #{ summary => "Get list of all available documents."
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"dbid">>
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


init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) -> {ok, Req, #{}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"POST">>, <<"GET">>],
  {Methods, Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, from_json}],
   Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

from_json(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  #{<<"source">> := SourceUrl,
    <<"target">> := TargetUrl} = jsx:decode(Body, [return_maps]),
  SourceConn = {barrel_httpc, SourceUrl},
  TargetConn = {barrel_httpc, TargetUrl},
  {ok, _Pid} = barrel:start_replication(SourceConn, TargetConn, []),
  RespBody = jsx:encode(#{ok => true}),
  Req3 = cowboy_req:set_resp_body(RespBody, Req2),
  {true, Req3, State}.

to_json(Req, State) ->
  {_RepId, Req2} = cowboy_req:binding(repid, Req),
  Reply = [],
  Json = jsx:encode(Reply),
  {Json, Req2, State}.
