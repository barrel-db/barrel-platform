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

-module(barrel_http_access_log).
-author("Bernard Notarianni").
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
	Path = cowboy_req:path_info(Req),
  Method = cowboy_req:method(Req),
  Path2 = case Path of
            undefined -> [];
            Path -> Path
          end,
  access:info("~s ~s", [Method, [<<"/">>, barrel_lib:binary_join(Path2, <<"/">>)]]),
	{ok, Req, Env}.

