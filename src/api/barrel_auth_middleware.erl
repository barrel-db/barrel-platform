
%% Copyright (c) 2016. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Created by benoitc on 13/06/16.

-module(barrel_auth_middleware).
-author("Benoit Chesneau").

%% API
-export([execute/2]).

%% TODO: convert to hook system
execute(Req, Env) ->
  Handlers = [list_to_atom(M) || M <- barrel_config:get_list("auth", "handlers", [])],
  case run_handlers(Handlers, Req, Env) of
    nil ->
      case barrel_config:get_boolean("auth", "require_valid_user", false) of
        true ->
          {ok, Req2} = cowboy_req:reply(411, Req),
          {halt, Req2};
        false ->
          {ok, Req, Env}
      end;
    {error, unauthorized} ->
      {ok, Req2} = cowboy_req:reply(411, Req),
      {halt, Req2};
    {ok, UserCtx, Req2, Env} ->
      Req3 = cowboy_req:set_meta(user_ctx, UserCtx, Req2),
      {ok, Req3, Env}
  end.


run_handlers([Handler | Rest], Req, Env) ->
  case Handler:authenticate(Req, Env) of
    nil -> run_handlers(Rest, Req, Env);
    Else -> Else
  end;
run_handlers([], _Req, _Env) -> nil.
