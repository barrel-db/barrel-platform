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

-module(barrel_http_rest_system).
-author("Bernard Notarianni").

-export([init/2]).

-record(state, {conn, method, database, docid, doc}).

init(Req, _Opts) ->
  barrel_monitor_activity:start(barrel_http_lib:backend_info(Req, system)),
  Method = cowboy_req:method(Req),
  check_database(Req, #state{method=Method}).

check_database(Req, State) ->
  Database = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    true ->
      DocId = cowboy_req:binding(docid, Req),
      State2 = State#state{database=Database,  docid=DocId},
      route(Req, State2);
    false ->
      barrel_http_reply:error(404, <<"database not found: ", Database/binary>>, Req, State)
  end.

route(Req, #state{method= <<"PUT">>, database=Db}=State) ->
  barrel_monitor_activity:update(#{ state => active, query => update_system_doc, db => Db }),
  create_resource(Req, State);
route(Req, #state{method= <<"GET">>, database=Db}=State) ->
  barrel_monitor_activity:update(#{ state => active, query => get_system_doc, db => Db }),
  check_resource_exists(Req, State);
route(Req, #state{method= <<"DELETE">>, database=Db}=State) ->
  barrel_monitor_activity:update(#{ state => active, query => update_system_doc, db => Db }),
  check_resource_exists(Req, State);
route(Req, State) ->
  barrel_http_reply:code(405, Req, State).

check_resource_exists(Req, State = #state{ database=Database, docid=DocId}) ->
  case barrel_db:get_system_doc(Database, DocId) of
    {ok, Doc} ->
      route2(Req, State#state{doc=Doc});
    {error, not_found} ->
      barrel_http_reply:error(404, Req, State)
  end.

route2(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
route2(Req, #state{method= <<"DELETE">>}=State) ->
  delete_resource(Req, State).

get_resource(Req, #state{doc=Doc}=State) ->
  barrel_http_reply:doc(Doc, Req, State).

create_resource(Req, State = #state{database=Database, docid=DocId}) ->
  {ok, Body, Req2} = cowboy_req:read_body(Req),
  Doc = jsx:decode(Body, [return_maps]),
  ok = barrel_db:put_system_doc(Database, DocId, Doc),
  barrel_http_reply:doc(#{ok => true}, Req2, State).

delete_resource(Req, State = #state{database=Database, docid=DocId}) ->
  ok = barrel_db:delete_system_doc(Database, DocId),
  barrel_http_reply:doc(#{ok => true}, Req, State).
