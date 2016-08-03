
%% Copyright (c) 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.
%%

%% Created by benoitc on 29/06/16.

-module(barrel_cb_all_dbs).
-author("Benoit Chesneau").

%% API
-export([init/3]).
-export([rest_init/2]).

-export([allowed_methods/2, content_types_provided/2, to_json/2]).

-define(DEFAULT_LIMIT, 16#10000000).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) -> {ok, Req, #{}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"GET">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

to_json(Req, State) ->
  {Limit, Req1} = cowboy_req:qs_val(<<"limit">>, Req, ?DEFAULT_LIMIT),
  {Skip, Req2} = cowboy_req:qs_val(<<"skip">>, Req1, -1),
  {ok, {DbNames, _, _}} = barrel:fold_databases(fun all_dbs_fun/2,
    {[], barrel_lib:to_integer(Skip), barrel_lib:to_integer(Limit)}),
  {jsx:encode(lists:usort(DbNames)), Req2, State}.

all_dbs_fun(_DbName, {Acc, Skip, 0}) ->
  {stop, {Acc, Skip, 0}};
all_dbs_fun(DbName, {Acc, 0, Limit}) ->
  {ok, {[barrel_lib:to_binary(DbName) | Acc], 0, Limit - 1}};
all_dbs_fun(_DbName, {Acc, Skip, Limit}) when Skip > 0 ->
  {ok, {Acc, Skip - 1, Limit}};
all_dbs_fun (DbName, {Acc, Skip, Limit}) ->
  {ok, {[barrel_lib:to_binary(DbName) | Acc], Skip, Limit - 1}}.
