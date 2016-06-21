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

-module(barrel_session_handler).
-author("Benoit Chesneau").

%% API
-export([init/3]).

-export([rest_init/2]).

-export([allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2,
         from_form/2,
         from_json/2,
         delete_resource/2,
         delete_completed/2]).


init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Env) ->
  {UserCtx, _} = cowboy_req:meta(user_ctx, Req, barrel_lib:userctx()),
  {ok, Req, #{user_ctx => UserCtx}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"GET">>, <<"DELETE">>, <<"POST">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

content_types_accepted(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, '*'}, from_json},
            {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, from_form}],
  {CTypes, Req, State}.


is_authorized(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  case Method of
    <<"GET">> ->
      #{ user_ctx := UserCtx } = State,
      ForceLogin = cowboy_req:qs_val(<<"basic">>, Req) =:= <<"true">>,
      case barrel_lib:userctx_get(name, UserCtx) of
        null when ForceLogin =:= true -> {false, Req2, State};
        _ -> {true, Req2, State}
      end;
    _ ->
      {true, Req2, State}
  end.

to_json(Req, State) ->
  #{ user_ctx := UserCtx } = State,
  [Name, Roles] = barrel_lib:userctx_get([name, roles], UserCtx),
  Reply = #{ ok => true, <<"userCtx">> => #{name => Name, roles => Roles}},
  {jsx:encode(Reply), Req, State}.

from_form(Req, State) ->
  {ok, Form, Req2} = cowboy_req:body_qs(Req),
  create_session(Form, Req2, State).

from_json(Req, State) ->
  {ok, Bin, Req2} = cowboy_req:body(Req),
  Form = jsx:decode(Bin),
  create_session(Form, Req2, State).

create_session(Form, Req, State) ->
  UserName = proplists:get_value(<<"name">>, Form, <<>>),
  Password = proplists:get_value(<<"password">>, Form, <<>>),
  User = case barrel_auth_cache:get_user_creds(UserName) of
           nil -> #{};
           Res -> Res
         end,
  UserSalt = maps:get(<<"salt">>, User, <<>>),
  case barrel_basic_auth:check_password(Password, User) of
    true ->
      Secret = barrel_auth:secret(),
      FullSecret = <<Secret/binary, UserSalt/binary>>,
      Req2 = barrel_cookie_auth:set_cookie_header(Req, UserName, FullSecret, true),
      Resp = jsx:encode(#{ok => true, name => maps:get(<<"name">>, User, null),
        roles => maps:get(<<"roles">>, User, [])}),

      Req3 = cowboy_req:set_resp_body(Resp, Req2),
      {Next, _} = cowboy_req:qs_val(<<"next">>, Req3),
      case Next of
        undefined ->
          {ok, _Req4} = cowboy_req:reply(200, Req3);
        _ ->
          {ok, _Req4} = cowboy_req:reply(302, [{<<"Location">>, Next}])
      end;
    false ->
      {ok, Req2} = cowboy_req:set_resp_cookie(<<"AuthSession">>, "",
        [{path, "/"}, {http_only, true}] + barrel_cookie_auth:secure(Req), Req),
      Fail = cowboy_req:qs_val(<<"fail">>, Req2),
      case Fail of
        undefined ->
          {ok, _Req3} = cowboy_req:reply(401, Req2);
        _ ->
          {ok, _Req3} = cowboy_req:reply(302, [{<<"Location">>, Fail}])
      end
  end,
  {halt, State}.

delete_resource(Req, State) ->
  {ok, Req2} = cowboy_req:set_resp_cookie(<<"AuthSession">>, "",
    [{path, "/"}, {http_only, true}] + barrel_cookie_auth:secure(Req), Req),
  Req3 = cowboy_req:set_resp_body(jsx:encode(#{ok => true}), Req2),
  Next = cowboy_req:qs_val(<<"next">>, Req3),
  case Next of
    undefined ->
      {ok, _Req4} = cowboy_req:reply(200, Req3);
    _ ->
      {ok, _Req4} = cowboy_req:reply(302, [{<<"Location">>, Next}])
  end,
  {true, Req, State}.

delete_completed(Req, State) -> {true, Req, State}.
