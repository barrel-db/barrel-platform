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
         create_session/2,
         delete_resource/2,
         delete_completed/2]).


init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Env) ->
  {UserCtx, _} = cowboy_req:meta(user_ctx, Req, barrel_lib:userctx()),
  {ok, Req, {UserCtx, nil}}.

allowed_methods(Req, State) ->
  Methods = [<<"HEAD">>, <<"OPTIONS">>, <<"GET">>, <<"DELETE">>, <<"POST">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, []}, to_json}],
  {CTypes, Req, State}.

content_types_accepted(Req, State) ->
  CTypes = [{{<<"application">>, <<"json">>, '*'}, create_session},
            {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, create_session}],
  {CTypes, Req, State}.


is_authorized(Req, {UserCtx, _} = State) ->
  {Method, Req2} = cowboy_req:method(Req),
  case Method of
    <<"GET">> ->
      ForceLogin = cowboy_req:qs_val(<<"basic">>, Req) =:= <<"true">>,
      case barrel_lib:userctx_get(name, UserCtx) of
        null when ForceLogin =:= true -> {false, Req2, State};
        _ -> {true, Req2, State}
      end;
    <<"POST">> ->
      case cowboy_req:parse_header(<<"content-type">>, Req2) of
        {ok,{<<"application">>,<<"x-www-form-urlencoded">>, _}, Req3} ->
          {ok, Form, Req4} = cowboy_req:body_qs(Req3),
          case check_session(Form, State) of
            {true, NewState} -> {true, Req4, NewState};
            {false, _} -> {false, Req4, State}
          end;
        {ok, {<<"application">>, <<"json">>, _}, Req3} ->
          {ok, Bin, Req4} = cowboy_req:body(Req3),
          Form = jsx:decode(Bin),
          case check_session(Form, State) of
            {true, NewState} -> {true, Req4, NewState};
            {false, _} -> {false, Req4, State}
          end;
        _Else ->
          io:format("got else ~p~n", [_Else]),
          {true, Req2, State}
       end;
    _ ->
      {true, Req2, State}
  end.

check_session(Form, {UserCtx, _}) ->
  UserName = proplists:get_value(<<"name">>, Form, <<>>),
  Password = proplists:get_value(<<"password">>, Form, <<>>),
  User = case barrel_auth_cache:get_user_creds(UserName) of
           nil -> #{};
           Res -> Res
         end,
  case barrel_basic_auth:check_password(Password, User) of
    true -> {true, {UserCtx, User#{<<"name">> => UserName}}};
    false -> {false, {UserCtx, nil}}
  end.



to_json(Req, {UserCtx, _} = State) ->
  [Name, Roles] = barrel_lib:userctx_get([name, roles], UserCtx),
  Reply = #{
    ok => true,
    <<"userCtx">> => #{name => Name, roles => Roles},
    <<"info">> => #{<<"authentication_db">> => <<"_users">>}
  },
  {jsx:encode(Reply), Req, State}.


create_session(Req, {_, User}=State) ->
  UserSalt = maps:get(<<"salt">>, User, <<>>),
  UserName = maps:get(<<"name">>, User),
  Secret = barrel_auth:secret(),
  FullSecret = <<Secret/binary, UserSalt/binary>>,
  Req2 = barrel_cookie_auth:set_cookie_header(Req, UserName, FullSecret, true),
  Resp = jsx:encode(#{ok => true, name => maps:get(<<"name">>, User, null),
    roles => maps:get(<<"roles">>, User, [])}),

  Req3 = cowboy_req:set_resp_body(Resp, Req2),
  {Next, _} = cowboy_req:qs_val(<<"next">>, Req3),
  case Next of
    undefined ->
      {true, Req3, State};
    _ ->
      {true, Next, Req3, State}
  end.

delete_resource(Req, State) ->
  Req2 = cowboy_req:set_resp_cookie(<<"AuthSession">>, <<"">>,
    [{path, "/"}, {http_only, true}, {max_age, 0}] ++ barrel_cookie_auth:secure(Req), Req),
  Req3 = cowboy_req:set_resp_body(jsx:encode(#{ok => true}), Req2),
  {Next, Req4} = cowboy_req:qs_val(<<"next">>, Req3),
  case Next of
    undefined ->
      {true, Req4, State};
    _ ->
      {ok, Req5} = cowboy_req:reply(302, [{<<"Location">>, Next}], Req4),
      {halt, Req5, State}
  end.

delete_completed(Req, State) -> {true, Req, State}.
