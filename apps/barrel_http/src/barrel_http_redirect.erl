%% Copyright 2017, Bernard Notarianni
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

-module(barrel_http_redirect).
-author("Bernard Notarianni").

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req, Opts) ->
  Location = proplists:get_value(location, Opts),
  {ok, Reply} = cowboy_req:reply(302, [{<<"Location">>, Location}], <<>>, Req),
  {ok, Reply, Opts}.

terminate(_Reason, _Req, _State) ->
  ok.
