%% Copyright 2016, Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://join_binaryup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

-module(barrel_server).

-export([node_info/0,
         node_id/0,
         version/0,
         node_name/0,
         cluster_name/0]).

-define(CLUSTER_NAME, <<"barrel">>).


node_info() ->
  #{ <<"name">> => node_name(),
     <<"cluster_name">> => cluster_name(),
     <<"uuid">> => node_id(),
     <<"version">> => #{ <<"number">> => list_to_binary(version()) }
   }.

node_name() -> [Name|_] = re:split(atom_to_list(node()), "@"), Name.

cluster_name() -> application:get_env(barrel, cluster_name, ?CLUSTER_NAME).

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
