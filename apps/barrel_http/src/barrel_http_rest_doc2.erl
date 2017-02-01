%% Copyright 2017, Bernard Notarianni
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

-module(barrel_http_rest_doc2).
-author("Bernard Notarianni").

%% API
-export([init/3]).
-export([handle/2]).
-export([info/3]).
-export([terminate/3]).

-export([trails/0]).

-include("barrel_http_rest_doc.hrl").

trails() ->
  barrel_http_rest_doc:trails(?MODULE).

init(Type, Req, []) ->
  barrel_http_rest_doc:init(Type, Req, []).

handle(Req, State) ->
  check_database_db(Req, State).

check_database_db(Req, State) ->
  {Database, Req2} = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    false ->
      barrel_http_reply:error(400, <<"database not found: ", Database/binary>>, Req2, State);
    true ->
      {Method, Req3} = cowboy_req:method(Req2),
      {DocId, Req4} = cowboy_req:binding(docid, Req3),
      State2 =  State#state{
                  database=Database,
                  docid=DocId,
                  method=Method
                 },
      route_all_docs(Req4, State2)
  end.

route_all_docs(Req, #state{method= <<"GET">>, database=Database, docid=undefined}=State) ->
  barrel_http_rest_docs_list:get_resource(Database, Req, State);
route_all_docs(Req, State) ->
  barrel_http_rest_doc:handle(Req, State).

info(_Message, Req, State) ->
  {loop, Req, State}.

terminate(Reason, Req, State) ->
  barrel_http_rest_doc:terminate(Reason, Req, State).
