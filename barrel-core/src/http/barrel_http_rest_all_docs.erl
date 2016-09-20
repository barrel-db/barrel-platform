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


init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) -> {ok, Req, #{}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"GET">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

to_json(Req, State) ->
  {DbId, Req2} = cowboy_req:binding(dbid, Req),
  Fun = fun(DocId, DocInfo, _Doc, Acc1) ->
            Rev = maps:get(current_rev, DocInfo),
            Row = #{<<"id">> => DocId,
                    <<"rev">> => Rev},
            {ok, [Row | Acc1]}
        end,
  Rows = barrel_db:fold_by_id(DbId, Fun, [], []),
  OffSet = 0,
  TotalRows = length(Rows),
  Reply = #{<<"offset">> => OffSet,
            <<"rows">> => Rows,
            <<"total_rows">> => TotalRows},
  barrel_http_reply:doc(Reply, Req2, State).

