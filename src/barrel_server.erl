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


-module(barrel_server).

-export([node_info/0,
         node_id/0,
         version/0,
         node_name/0]).

node_info() ->
  #{ <<"name">> => <<"barrel">>,
     <<"cluster_name">> => node_name(),
     <<"uuid">> => node_id(),
     <<"version">> => #{ <<"number">> => list_to_binary(version()) }
   }.

node_name() -> [Name|_] = re:split(atom_to_list(node()), "@"), Name.


node_id() ->
  case barrel_config:get("barrel", "uuid", nil) of
    nil ->
      Uuid = barrel_uuids:random(),
      barrel_config:set("barrel", "uuid", Uuid),
      Uuid;
    Uuid ->
      barrel_lib:to_binary(Uuid)
  end.

version() -> {ok, Vsn} = application:get_key(barrel, vsn), Vsn.
