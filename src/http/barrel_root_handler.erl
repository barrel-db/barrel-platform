%% Copyright 2016, Benoit Chesneau
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

-module(barrel_root_handler).

-export([init/3]).
-export([rest_init/2]).

-export([allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) -> {ok, Req, #{}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"GET">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

to_json(Req, State) ->
  NodeInfo = barrel_server:node_info(),
  {jsx:encode(NodeInfo), Req, State}.
