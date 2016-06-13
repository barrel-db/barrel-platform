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

-module(barrel_cookie_auth).
-author("Benoit Chesneau").

%% API
-export([authenticate/2]).

%% internal
-export([cookie_time/0]).
-export([max_age/0]).
-export([set_cookie_header/4]).
-export([secure/1]).

authenticate(Req, Env) ->
  case cowboy_req:cookie(<<"AuthSession">>, Req) of
    {undefined, _} -> nil;
    {Cookie, Req2} ->
      [User, TimeBin, Hash] = try decode_auth_cookie(Cookie)
                                 catch
                                   _:_ ->
                                     Reason = <<"Malformed AuthSession cookie. Please clear your cookies.">>,
                                     {error, {bad_request, Reason}}
                                 end,
      Current = cookie_time(),
      case barrel_config:get_binary("auth", "cookie_secret") of
        nil ->
          lager:debug("cookie auth secret is not set.~n", []),
          nil;
        Secret ->
          case couch_auth_cache:get_user_creds(User) of
            nil -> nil;
            UserProps ->
              UserSalt = maps:get(<<"salt">>, UserProps, <<>>),
              FullSecret = <<Secret/binary, UserSalt/binary>>,
              ExpectedHash = crypto:hmac(sha, FullSecret, User ++ ":" ++ TimeBin),
              Timeout = barrel_config:get_integer("auth", "timeout", 600),
              case (catch binary_to_integer(TimeBin, 16)) of
                Timestamp when Current < Timestamp + Timeout ->
                  case couch_passwords:verify(ExpectedHash, Hash) of
                    true ->
                      TimeLeft = Timestamp + Timeout - Current,
                      ResetCookie = TimeLeft < Timeout*0.9,
                      UserCtx = barrel_lib:userctx([{name, list_to_binary(User)},
                        {roles, maps:get(<<"roles">>, UserProps, [])},
                        {auth, {FullSecret, ResetCookie}}]),
                      Req3 = set_cookie_header(Req2, User, FullSecret, ResetCookie),
                      {ok, UserCtx, Req3, Env};
                    false ->
                      nil
                  end;
                _ ->
                  nil
              end
          end
      end
  end.

set_cookie_header(Req, _User, _Secret, false) -> Req;
set_cookie_header(Req, User, Secret, true) ->
  Timestamp = cookie_time(),
  SessionData = << User/binary, ":", (integer_to_binary(Timestamp,16))/binary >>,
  Hash = crypto:hmac(sha, Secret, SessionData),
  cowboy_req:set_resp_cookie(<<"AuthSession">>,   barrel_lib:encodeb64url(<< SessionData/binary, ":", Hash/binary>>),
    [{path, <<"/">>}, {http_only, true}] ++ secure(Req) ++ max_age(), Req).


secure(Req) ->
  case cowboy_req:get(transport, Req) of
    ranch_tcp -> [];
    ranch_ssl -> [{secure, true}]
  end.

max_age() ->
  case barrel_config:get_boolean("auth", "allow_persistent_cookies", false) of
    false -> [];
    true -> [{max_age, barrel_config:get_integer("auth", "timeout", 600)}]
  end.

cookie_time() -> {NowMS, NowS, _} = erlang:timestamp(), NowMS * 1000000 + NowS.

decode_auth_cookie(Cookie) ->
  AuthSession = barrel_lib:decodeb64url(Cookie),
  binary:split(AuthSession, <<":">>, [global]).
