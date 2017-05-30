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

-export([req/1, req/2, req/3, req/4]).
-export([log/2]).


req(Request) when is_map(Request) ->
  Method = maps:get(method, Request, get),
  Route = maps:get(route, Request),
  Headers = maps:get(headers, Request, []),
  Body = case maps:get(body, Request, <<>>) of
           Map when is_map(Map) ->
             jsx:encode(Map);
           Bin when is_binary(Bin) -> Bin
         end,
  Url = <<"http://localhost:7080", (list_to_binary(Route))/binary>>,
  H = [{<<"Content-Type">>, <<"application/json">>} | Headers],
  case hackney:request(Method, Url, H, Body, []) of
    {ok, Code, RespHeaders, Ref} ->
      {ok, Answer} = hackney:body(Ref),
      #{code => Code,
        body => Answer,
        doc => jsx:decode(Answer, [return_maps]),
        headers => RespHeaders};
    Error -> Error
  end.

req(Method, Route) ->
  req(Method,Route,[]).

req(Method, Route, Map) when is_map(Map) ->
  Body = jsx:encode(Map),
  req(Method, Route, Body);

req(Method, Route, String) when is_list(String) ->
  Body = list_to_binary(String),
  req(Method, Route, Body);

req(Method, Route, Body) when is_binary(Body) ->
  Server = "http://localhost:7080",
  Path = list_to_binary(Server ++ Route),
  barrel_http_lib:req(Method, Path, Body).

req(Method, Route, Body, Rev) when is_binary(Body), is_binary(Rev) ->
  Server = "http://localhost:7080",
  Path = list_to_binary(Server ++ Route),
  barrel_http_lib:req(Method, Path, Body, Rev).

log(String, Params) ->
  Line = io_lib:fwrite(String, Params),
  Pid = io_lib:fwrite("~p",[self()]),
  {ok, File} = file:open("/tmp/test.txt", [append]),
  ok = file:write(File, Pid ++ ";" ++ Line),
  ok = file:close(File),
  ok.
