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
%%
%% @doc basic authentication middleware

%% Created by benoitc on 13/06/16.

-module(barrel_basic_auth).
-author("Benoit Chesneau").

%% API
-export([authenticate/2]).

-export([check_password/2]).

authenticate(Req, Env) ->
  case get_credentials(Req) of
    nil -> nil;
    {User, Pass} ->
      case couch_auth_cache:get_user_creds(User) of
        nil -> {error, unauthorized};
        UserProps ->
          case check_password(Pass, UserProps) of
            true ->
              UserCtx =  barrel_lib:userctx([{name, User},
                {roles, maps:get(<<"roles">>, UserProps, [])}]),
              {ok, UserCtx, Req, Env};
            false ->
              {error, unauthorized}
          end
      end
  end.


get_credentials(Req) ->
  case cowboy_req:header(<<"authorization">>, Req) of
    <<"Basic ", BinCreds/binary>> ->
      case binary:split(BinCreds, <<":">>) of
        [<<"_">>, <<"_">>] -> nil;  % special name and pass to be logged out
        [User, Pass] -> {User, Pass};
        _ -> nil
      end;
    _ -> nil
  end.

check_password(Pass, UserProps) ->
  UserSalt = maps:get(<<"salt">>, UserProps, <<>>),
  {PasswordHash, ExpectedHash} =  case maps:get(<<"password_scheme">>, UserProps, <<"simple">>) of
                                    <<"simple">> ->
                                      {couch_passwords:simple(Pass, UserSalt),
                                        maps:get(<<"password_sha">>, UserProps, nil)};
                                    <<"pbkdf2">> ->
                                      Iterations = maps:get(<<"iterations">>, UserProps, 10000),
                                      {couch_passwords:pbkdf2(Pass, UserSalt, Iterations),
                                        maps:get(<<"derived_key">>, UserProps, nil)}
                                  end,
  couch_passwords:verify(PasswordHash, ExpectedHash).
