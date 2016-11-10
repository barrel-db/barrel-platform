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
-export([rest_init/2]).

-export([allowed_methods/2, content_types_provided/2, to_json/2]).

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


init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) -> {ok, Req, #{}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"GET">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

to_json(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  {DbId, Req3} = cowboy_req:binding(dbid, Req2),
  Fun = fun(DocId, DocInfo, _Doc, Acc1) ->
            Rev = maps:get(current_rev, DocInfo),
            Row = #{<<"id">> => DocId,
                    <<"rev">> => Rev},
            {ok, [Row | Acc1]}
        end,
  Conn = barrel_http_conn:peer(Store, DbId),
  Rows = barrel_db:fold_by_id(Conn, Fun, [], []),
  OffSet = 0,
  TotalRows = length(Rows),
  Reply = #{<<"offset">> => OffSet,
            <<"rows">> => Rows,
            <<"total_rows">> => TotalRows},
  Json = jsx:encode(Reply),
  {Json, Req3, State}.
