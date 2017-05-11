%% Copyright 2017 Benoit Chesneau
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
-module(barrel_monitor_exporter_handler).
-author("benoitc").

%% API
-export([
  init/2,
  terminate/3
]).

%%--------------------------------------------------------------------
init(Req, Opts) ->
  handle(Req, Opts).

handle(Req, {Registry}) ->
  URI = true,
  GetHeader =
    fun(Name, Default) ->
      cowboy_req:header(iolist_to_binary(Name), Req, Default)
    end,

  %% TODO: check method, response only to GET
  {Code, RespHeaders0, Body} = prometheus_http:reply(
    #{path => URI,
      headers => GetHeader,
      registry => Registry,
      standalone => false}),

  ContentLength = integer_to_list(iolist_size(Body)),
  RespHeaders = lists:map(fun to_cowboy_headers/1,  RespHeaders0 ++ [{content_length, ContentLength}]),
  Req2 = cowboy_req:reply(Code, maps:from_list(RespHeaders), Body, Req),
  {ok, Req2, undefined}.

terminate(_Reason, _Req, _State) ->
  ok.

to_cowboy_headers({Name, Value}) ->
  {to_cowboy_name(Name), Value}.

to_cowboy_name(Name) ->
  binary:replace(atom_to_binary(Name, utf8), <<"_">>, <<"-">>).