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

-module(barrel_http_rest_stores).
-author("Bernard Notarianni").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Get list of available stores"
               , produces => ["application/json"]
               }
     },
  [trails:trail("/_stores/", ?MODULE, [], Metadata)].

-record(state, {method, store, databases, dbid}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

route(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).


get_resource(Req, State) ->
  {ok, Stores} = application:get_env(barrel, stores),
  Doc = [ create_info(Store, Options) || {Store, Options} <- Stores],
  barrel_http_reply:doc(Doc, Req, State).

create_info(Store, Options) ->
  #{<<"name">> => list_to_binary(atom_to_list(Store)),
    <<"dir">> => list_to_binary(maps:get(dir, Options))}.
