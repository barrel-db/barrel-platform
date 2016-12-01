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

-module(test_lib).

-export([req/2, req/3]).
-export([log/2]).


req(Method, Route) ->
  req(Method,Route,[]).

req(Method, Route, Map) when is_map(Map) ->
  Body = jsx:encode(Map),
  req(Method, Route, Body);

req(Method, Route, String) when is_list(String) ->
  Body = list_to_binary(String),
  req(Method, Route, Body);

req(Method, Route, Body) when is_binary(Body) ->
  Server = "http://localhost:8080",
  Path = list_to_binary(Server ++ Route),
  barrel_http_lib:req(Method, Path, Body).


log(String, Params) ->
  Line = io_lib:fwrite(String, Params),
  Pid = io_lib:fwrite("~p",[self()]),
  {ok, File} = file:open("/tmp/test.txt", [append]),
  ok = file:write(File, Pid ++ ";" ++ Line),
  ok = file:close(File),
  ok.
