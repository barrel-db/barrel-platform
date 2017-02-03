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

-module(barrel_http_rest_dbs).
-author("Bernard Notarianni").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Get list of available databases"
               , produces => ["application/json"]
               },
       post => #{ summary => "Create a new database"
                , produces => ["application/json"]
                , responses =>
                    #{ <<"200">> => #{ description => "Database created" }
                       }
                , parameters =>
                    [#{ name => <<"body">>
                      , description => <<"New database configuration">>
                      , in => <<"body">>
                      , required => true
                      , type => <<"json">>}
                    ]
                }
     },
  [trails:trail("/dbs/", ?MODULE, [], Metadata)].

-record(state, {method, body}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

route(Req, #state{method= <<"GET">>}=State) ->
  get_resource(Req, State);
route(Req, #state{method= <<"POST">>}=State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  check_body(Req2, State#state{body=Body});
route(Req, State) ->
  barrel_http_reply:error(405, Req, State).


get_resource(Req, State) ->
  Dbs = barrel_store:databases(),
  barrel_http_reply:doc(Dbs, Req, State).


check_body(Req, #state{body= <<>>}=S) ->
  barrel_http_reply:error(400, <<"empty body">>, Req, S);
check_body(Req, #state{body=Body}=S) ->
  try jsx:decode(Body, [return_maps]) of
      Json ->
      create_resource(Req, S#state{body=Json})
  catch
    _:_ ->
      barrel_http_reply:error(400, <<"malformed json document for database config">>, Req, S)
  end.

create_resource(Req, #state{body=Json}=State) ->
  case barrel:create_db(Json) of
    {ok, Config} ->
      barrel_http_reply:json(201, Config, Req, State);
    {error, db_exists} ->
      barrel_http_reply:error(409, "db exists", Req, State);
    Error ->
      lager:error("got server error ~p~n", [Error]),
      barrel_http_reply:error(500, "db error", Req, State)
  end.

