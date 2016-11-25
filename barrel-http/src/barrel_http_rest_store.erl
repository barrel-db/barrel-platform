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

-module(barrel_http_rest_store).
-author("Bernard Notarianni").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Get list of available databases of a store"
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               }
     },
  [trails:trail("/_store/:store", ?MODULE, [], Metadata)].

-record(state, {method, store}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

route(Req, #state{method= <<"GET">>}=State) ->
  check_store_exist(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).

check_store_exist(Req, State) ->
  {StoreBin, Req2} = cowboy_req:binding(store, Req),
  Store = list_to_atom(binary_to_list(StoreBin)),
  {ok, Stores} = application:get_env(barrel, stores),
  case proplists:is_defined(Store, Stores) of
    true ->
      get_resource(Req2, State#state{store=Store});
    false ->
      barrel_http_reply:error(404, "store not found", Req2, State)
  end.


get_resource(Req, #state{store=Store}=State) ->
  Databases =  barrel:database_names(Store),
  Doc = [database_info(Store, DbName) || DbName <- Databases],
  barrel_http_reply:doc(Doc, Req, State).

database_info(Store, DbName) ->
  {ok, Conn} = barrel:connect_database(Store, DbName),
  {ok, Infos} = barrel:database_infos(Conn),
  Infos.
