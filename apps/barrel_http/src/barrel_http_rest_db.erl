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

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Get the database informations"
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"database">>
                     , description => <<"Database ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               },
      delete => #{ summary => "Delete a database"
        , produces => ["application/json"]
        , parameters =>
        [#{ name => <<"database">>
          , description => <<"Database ID">>
          , in => <<"path">>
          , required => true
          , type => <<"string">>}
        ]
      }
    },
  [trails:trail("/dbs/:database", ?MODULE, [], Metadata)].

-record(state, {method, database}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

route(Req, #state{method= <<"HEAD">>}=State) ->
  {Database, Req2} = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    true ->
      barrel_http_reply:json(200, <<>>, Req2, State);
    false ->
      barrel_http_reply:error(404, <<>>, Req2, State)
  end;
route(Req, #state{method= <<"GET">>}=State) ->
  check_database_exist(Req, State);
route(Req, #state{method= <<"DELETE">>}=State) ->
  {Database, Req2} = cowboy_req:binding(database, Req),
  ok = barrel_local:delete_db(Database),
  barrel_http_reply:json(200, #{ ok => true }, Req2, State);
route(Req, #state{method= <<"POST">>}) ->
  barrel_http_rest_doc:handle_post(Req);
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).

check_database_exist(Req, State) ->
  {Database, Req2} = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    true ->
      get_resource(Req2, State#state{database=Database});
    false ->
      barrel_http_reply:error(404, "database not found", Req2, State)
  end.

get_resource(Req, #state{database=Database}=State) ->
  barrel_http_reply:doc(barrel_local:db_infos(Database), Req, State).
