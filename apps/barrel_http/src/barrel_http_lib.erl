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

-module(barrel_http_lib).

-export([req/2, req/3]).
-export([has_store/1]).
-export([has_database/1]).

req(Method, Url) ->
  req(Method,Url,[]).

req(Method, Url, Map) when is_map(Map) ->
  Body = jsx:encode(Map),
  req(Method, Url, Body);

req(Method, Url, String) when is_list(String) ->
  Body = list_to_binary(String),
  req(Method, Url, Body);

req(Method, Url, Body) when is_binary(Body) ->
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  case hackney:request(Method, Url, Headers, Body, []) of
    {ok, Code, _Headers, Ref} ->
      {ok, Answer} = hackney:body(Ref),
      {Code, Answer};
    Error -> Error
  end.

has_database(Database) ->
  has_store(Database).
has_store(Store) ->
  case barrel_store:whereis_db(Store) of
    undefined -> false;
    _Db -> true
  end.
