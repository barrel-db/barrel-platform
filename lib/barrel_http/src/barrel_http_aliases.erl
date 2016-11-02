
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

-module(barrel_http_aliases).
-author("Bernard Notarianni").

-export([init_ets/0]).
-export([add/1]).
-export([add/2]).
-export([get/1]).

init_ets() ->
  ets:new(barrel_http_aliases, [public, named_table]).

add(Aliases) ->
  [ok = add(Alias, Uri) || {Alias, Uri} <- Aliases],
  ok.

add(Alias, Uri) when is_list(Alias) ->
  add(list_to_binary(Alias), Uri);

add(Alias, Uri) when is_binary(Alias) ->
  true = ets:insert(barrel_http_aliases, {Alias, Uri}),
  ok.

get(Alias) ->
  case ets:lookup(barrel_http_aliases, Alias) of
    [] ->
      {error, {unknown_alias, Alias}};
    [{Alias, Uri}] ->
      {ok, Uri}
  end.
