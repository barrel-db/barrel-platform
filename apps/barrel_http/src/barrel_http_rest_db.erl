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

-module(barrel_http_rest_db).
-author("Bernard Notarianni").

-export([init/2]).
-export([terminate/3]).

-record(state, {method, database}).

init(Req, _Opts) ->
  Method = cowboy_req:method(Req),
  route(Req, #state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

route(Req, #state{method= <<"HEAD">>}=State) ->
  Database = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    true ->
      barrel_http_reply:json(200, <<>>, Req, State);
    false ->
      barrel_http_reply:error(404, <<>>, Req, State)
  end;
route(Req, #state{method= <<"GET">>}=State) ->
  check_database_exist(Req, State);
route(Req, #state{method= <<"DELETE">>}=State) ->
  Database = cowboy_req:binding(database, Req),
  ok = barrel_local:delete_db(Database),
  barrel_http_reply:json(200, #{ ok => true }, Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).

check_database_exist(Req, State) ->
  Database = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    true ->
      get_resource(Req, State#state{database=Database});
    false ->
      barrel_http_reply:error(404, "database not found", Req, State)
  end.

get_resource(Req, #state{database=Database}=State) ->
  barrel_http_reply:doc(barrel_local:db_infos(Database), Req, State).
