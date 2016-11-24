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

-module(barrel_http_rest_all_docs).
-author("Bernard Notarianni").


%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Get list of all available documents."
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"dbid">>
                     , description => <<"Database ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ,#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}
                   ]
               }
     },
  [trails:trail("/:store/:dbid/_all_docs", ?MODULE, [], Metadata)].

-record(state, {method, store, dbid, conn}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.


route(Req, #state{method= <<"GET">>}=State) ->
  check_store_db(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, "method not allowed", Req, State).


check_store_db(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  case barrel:connect_database(barrel_lib:to_atom(Store), DbId) of
    {error, {unknown_store, _}} ->
      barrel_http_reply:error(400, "store not found", Req, State);
    {error, not_found} ->
      barrel_http_reply:error(400, "database not found", Req, State);
    {ok, Conn} ->
      State2 = State#state{store=Store, dbid=DbId, conn=Conn},
      get_resource(Req3, State2)
  end.


get_resource(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  Fun = fun(DocId, DocInfo, _Doc, Acc1) ->
            Rev = maps:get(current_rev, DocInfo),
            Row = #{<<"id">> => DocId,
                    <<"rev">> => Rev},
            {ok, [Row | Acc1]}
        end,
  {ok, Conn} = barrel:connect_database(barrel_lib:to_atom(Store), DbId),
  Rows = barrel:fold_by_id(Conn, Fun, [], []),
  OffSet = 0,
  TotalRows = length(Rows),
  Reply = #{<<"offset">> => OffSet,
            <<"rows">> => Rows,
            <<"total_rows">> => TotalRows},
  barrel_http_reply:doc(Reply, Req3, State).
