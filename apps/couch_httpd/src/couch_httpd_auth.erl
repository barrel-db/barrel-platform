% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_auth).
-include_lib("couch/include/couch_db.hrl").

-export([default_authentication_handler/1,special_test_authentication_handler/1]).
-export([cookie_authentication_handler/1]).
-export([null_authentication_handler/1]).
-export([proxy_authentication_handler/1, proxy_authentification_handler/1]).
-export([cookie_auth_header/2]).
-export([handle_session_req/1]).

-import(couch_httpd, [header_value/2, send_json/2,send_json/4, send_method_not_allowed/2]).

special_test_authentication_handler(Req) ->
    case header_value(Req, "WWW-Authenticate") of
    "X-Couch-Test-Auth " ++ NamePass ->
        % NamePass is a colon separated string: "joe schmoe:a password".
        [Name, Pass] = re:split(NamePass, ":", [{return, list}, {parts, 2}]),
        case {Name, Pass} of
        {"Jan Lehnardt", "apple"} -> ok;
        {"Christopher Lenz", "dog food"} -> ok;
        {"Noah Slater", "biggiesmalls endian"} -> ok;
        {"Chris Anderson", "mp3"} -> ok;
        {"Damien Katz", "pecan pie"} -> ok;
        {_, _} ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
        end,
        Req#httpd{user_ctx=#user_ctx{name=list_to_binary(Name)}};
    _ ->
        % No X-Couch-Test-Auth credentials sent, give admin access so the
        % previous authentication can be restored after the test
        Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}
    end.

basic_name_pw(Req) ->
    AuthorizationHeader = header_value(Req, "Authorization"),
    case AuthorizationHeader of
    "Basic " ++ Base64Value ->
        case re:split(base64:decode(Base64Value), ":",
                      [{return, list}, {parts, 2}]) of
        ["_", "_"] ->
            % special name and pass to be logged out
            nil;
        [User, Pass] ->
            {User, Pass};
        _ ->
            nil
        end;
    _ ->
        nil
    end.

default_authentication_handler(Req) ->
    case basic_name_pw(Req) of
    {User, Pass} ->
        case couch_auth_cache:get_user_creds(User) of
            nil ->
                throw({unauthorized, <<"Name or password is incorrect.">>});
            UserProps ->
                case authenticate(list_to_binary(Pass), UserProps) of
                    true ->
                        Req#httpd{user_ctx=#user_ctx{
                            name=list_to_binary(User),
                            roles=couch_util:get_value(<<"roles">>, UserProps, [])
                        }};
                    _Else ->
                        throw({unauthorized, <<"Name or password is incorrect.">>})
                end
        end;
    nil ->
        case couch_server:has_admins() of
        true ->
            Req;
        false ->
            case barrel_config:get("couch_httpd_auth", "require_valid_user", "false") of
                "true" -> Req;
                % If no admins, and no user required, then everyone is admin!
                % Yay, admin party!
                _ -> Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}
            end
        end
    end.

null_authentication_handler(Req) ->
    Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}.

%% @doc proxy auth handler.
%
% This handler allows creation of a userCtx object from a user authenticated remotly.
% The client just pass specific headers to CouchDB and the handler create the userCtx.
% Headers  name can be defined in local.ini. By thefault they are :
%
%   * X-Auth-CouchDB-UserName : contain the username, (x_auth_username in
%   couch_httpd_auth section)
%   * X-Auth-CouchDB-Roles : contain the user roles, list of roles separated by a
%   comma (x_auth_roles in couch_httpd_auth section)
%   * X-Auth-CouchDB-Token : token to authenticate the authorization (x_auth_token
%   in couch_httpd_auth section). This token is an hmac-sha1 created from secret key
%   and username. The secret key should be the same in the client and couchdb node. s
%   ecret key is the secret key in couch_httpd_auth section of ini. This token is optional
%   if value of proxy_use_secret key in couch_httpd_auth section of ini isn't true.
%
proxy_authentication_handler(Req) ->
    case proxy_auth_user(Req) of
        nil -> Req;
        Req2 -> Req2
    end.

%% @deprecated
proxy_authentification_handler(Req) ->
    proxy_authentication_handler(Req).

proxy_auth_user(Req) ->
    XHeaderUserName = barrel_config:get("couch_httpd_auth", "x_auth_username", "X-Auth-CouchDB-UserName"),
    XHeaderRoles = barrel_config:get("couch_httpd_auth", "x_auth_roles", "X-Auth-CouchDB-Roles"),
    XHeaderToken = barrel_config:get("couch_httpd_auth", "x_auth_token", "X-Auth-CouchDB-Token"),
    case header_value(Req, XHeaderUserName) of
        undefined -> nil;
        UserName ->
            Roles = case header_value(Req, XHeaderRoles) of
                undefined -> [];
                Else ->
                    [list_to_binary(R) || R <- string:tokens(Else, ",")]
            end,
            case barrel_config:get("couch_httpd_auth", "proxy_use_secret", "false") of
                "true" ->
                    case barrel_config:get("couch_httpd_auth", "secret", nil) of
                        nil ->
                            Req#httpd{user_ctx=#user_ctx{name=list_to_binary(UserName), roles=Roles}};
                        Secret ->
                            ExpectedToken = couch_util:to_hex(crypto:hmac(sha, Secret, UserName)),
                            case header_value(Req, XHeaderToken) of
                                Token when Token == ExpectedToken ->
                                    Req#httpd{user_ctx=#user_ctx{name=list_to_binary(UserName),
                                                            roles=Roles}};
                                _ -> nil
                            end
                    end;
                _ ->
                    Req#httpd{user_ctx=#user_ctx{name=list_to_binary(UserName), roles=Roles}}
            end
    end.


cookie_authentication_handler(#httpd{mochi_req=MochiReq}=Req) ->
    case MochiReq:get_cookie_value("AuthSession") of
    undefined -> Req;
    [] -> Req;
    Cookie ->
        [User, TimeStr, HashStr] = try
            AuthSession = couch_util:decodeBase64Url(Cookie),
            [_A, _B, _Cs] = re:split(binary_to_list(AuthSession), ":",
                                     [{return, list}, {parts, 3}])
        catch
            _:_Error ->
                Reason = <<"Malformed AuthSession cookie. Please clear your cookies.">>,
                throw({bad_request, Reason})
        end,
        % Verify expiry and hash
        CurrentTime = make_cookie_time(),
        case barrel_config:get("couch_httpd_auth", "secret", nil) of
        nil ->
            barrel_log:debug("cookie auth secret is not set",[]),
            Req;
        SecretStr ->
            Secret = list_to_binary(SecretStr),
            case couch_auth_cache:get_user_creds(User) of
            nil -> Req;
            UserProps ->
                UserSalt = couch_util:get_value(<<"salt">>, UserProps, <<"">>),
                FullSecret = <<Secret/binary, UserSalt/binary>>,
                ExpectedHash = crypto:hmac(sha, FullSecret, User ++ ":" ++ TimeStr),
                Hash = list_to_binary(HashStr),
                Timeout = barrel_config:get_integer("couch_httpd_auth", "timeout", 600),
                barrel_log:debug("timeout ~p", [Timeout]),
                case (catch erlang:list_to_integer(TimeStr, 16)) of
                    TimeStamp when CurrentTime < TimeStamp + Timeout ->
                        case couch_passwords:verify(ExpectedHash, Hash) of
                            true ->
                                TimeLeft = TimeStamp + Timeout - CurrentTime,
                                barrel_log:debug("Successful cookie auth as: ~p", [User]),
                                Req#httpd{user_ctx=#user_ctx{
                                    name=list_to_binary(User),
                                    roles=couch_util:get_value(<<"roles">>, UserProps, [])
                                }, auth={FullSecret, TimeLeft < Timeout*0.9}};
                            _Else ->
                                Req
                        end;
                    _Else ->
                        Req
                end
            end
        end
    end.

cookie_auth_header(#httpd{user_ctx=#user_ctx{name=null}}, _Headers) -> [];
cookie_auth_header(#httpd{user_ctx=#user_ctx{name=User}, auth={Secret, true}}=Req, Headers) ->
    % Note: we only set the AuthSession cookie if:
    %  * a valid AuthSession cookie has been received
    %  * we are outside a 10% timeout window
    %  * and if an AuthSession cookie hasn't already been set e.g. by a login
    %    or logout handler.
    % The login and logout handlers need to set the AuthSession cookie
    % themselves.
    CookieHeader = couch_util:get_value("Set-Cookie", Headers, ""),
    Cookies = mochiweb_cookies:parse_cookie(CookieHeader),
    AuthSession = couch_util:get_value("AuthSession", Cookies),
    if AuthSession == undefined ->
        TimeStamp = make_cookie_time(),
        [cookie_auth_cookie(Req, binary_to_list(User), Secret, TimeStamp)];
    true ->
        []
    end;
cookie_auth_header(_Req, _Headers) -> [].

cookie_auth_cookie(Req, User, Secret, TimeStamp) ->
    SessionData = User ++ ":" ++ erlang:integer_to_list(TimeStamp, 16),
    Hash = crypto:hmac(sha, Secret, SessionData),
    mochiweb_cookies:cookie("AuthSession",
        couch_util:encodeBase64Url(SessionData ++ ":" ++ binary_to_list(Hash)),
        [{path, "/"}] ++ cookie_scheme(Req) ++ max_age()).

ensure_cookie_auth_secret() ->
    case barrel_config:get("couch_httpd_auth", "secret", nil) of
        nil ->
            NewSecret = binary_to_list(barrel_uuids:random()),
            barrel_config:set("couch_httpd_auth", "secret", NewSecret),
            NewSecret;
        Secret -> Secret
    end.

% session handlers
% Login handler with user db
handle_session_req(#httpd{method='POST', mochi_req=MochiReq}=Req) ->
    ReqBody = MochiReq:recv_body(),
    Form = case MochiReq:get_primary_header_value("content-type") of
        % content type should be json
        "application/x-www-form-urlencoded" ++ _ ->
            mochiweb_util:parse_qs(ReqBody);
        "application/json" ++ _ ->
            {Pairs} = ?JSON_DECODE(ReqBody),
            lists:map(fun({Key, Value}) ->
              {binary_to_list(Key), ?b2l(Value)}
            end, Pairs);
        _ ->
            []
    end,
    UserName = list_to_binary(couch_util:get_value("name", Form, "")),
    Password = list_to_binary(couch_util:get_value("password", Form, "")),
    barrel_log:debug("Attempt Login: ~s",[UserName]),
    User = case couch_auth_cache:get_user_creds(UserName) of
        nil -> [];
        Result -> Result
    end,
    UserSalt = couch_util:get_value(<<"salt">>, User, <<>>),
    case authenticate(Password, User) of
        true ->
            % setup the session cookie
            Secret = list_to_binary(ensure_cookie_auth_secret()),
            CurrentTime = make_cookie_time(),
            Cookie = cookie_auth_cookie(Req, binary_to_list(UserName), <<Secret/binary, UserSalt/binary>>, CurrentTime),
            % TODO document the "next" feature in Futon
            {Code, Headers} = case couch_httpd:qs_value(Req, "next", nil) of
                nil ->
                    {200, [Cookie]};
                Redirect ->
                    {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
            end,
            send_json(Req#httpd{req_body=ReqBody}, Code, Headers,
                {[
                    {ok, true},
                    {name, couch_util:get_value(<<"name">>, User, null)},
                    {roles, couch_util:get_value(<<"roles">>, User, [])}
                ]});
        _Else ->
            % clear the session
            Cookie = mochiweb_cookies:cookie("AuthSession", "", [{path, "/"}] ++ cookie_scheme(Req)),
            {Code, Headers} = case couch_httpd:qs_value(Req, "fail", nil) of
                nil ->
                    {401, [Cookie]};
                Redirect ->
                    {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
            end,
            send_json(Req, Code, Headers, {[{error, <<"unauthorized">>},{reason, <<"Name or password is incorrect.">>}]})
    end;
% get user info
% GET /_session
handle_session_req(#httpd{method='GET', user_ctx=UserCtx}=Req) ->
    Name = UserCtx#user_ctx.name,
    ForceLogin = couch_httpd:qs_value(Req, "basic", "false"),
    case {Name, ForceLogin} of
        {null, "true"} ->
            throw({unauthorized, <<"Please login.">>});
        {Name, _} ->
            send_json(Req, {[
                % remove this ok
                {ok, true},
                {<<"userCtx">>, {[
                    {name, Name},
                    {roles, UserCtx#user_ctx.roles}
                ]}},
                {info, {[
                    {authentication_db, barrel_config:get_binary("couch_httpd_auth", "authentication_db", <<"_users">>)},
                    {authentication_handlers, [auth_name(H) || H <- couch_httpd:make_fun_spec_strs(
                            barrel_config:get("httpd", "authentication_handlers"))]}
                ] ++ maybe_value(authenticated, UserCtx#user_ctx.handler, fun(Handler) ->
                        auth_name(binary_to_list(Handler))
                    end)}}
            ]})
    end;
% logout by deleting the session
handle_session_req(#httpd{method='DELETE'}=Req) ->
    Cookie = mochiweb_cookies:cookie("AuthSession", "", [{path, "/"}] ++ cookie_scheme(Req)),
    {Code, Headers} = case couch_httpd:qs_value(Req, "next", nil) of
        nil ->
            {200, [Cookie]};
        Redirect ->
            {302, [Cookie, {"Location", couch_httpd:absolute_uri(Req, Redirect)}]}
    end,
    send_json(Req, Code, Headers, {[{ok, true}]});
handle_session_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST,DELETE").

maybe_value(_Key, undefined, _Fun) -> [];
maybe_value(Key, Else, Fun) ->
    [{Key, Fun(Else)}].

authenticate(Pass, UserProps) ->
    UserSalt = couch_util:get_value(<<"salt">>, UserProps, <<>>),
    {PasswordHash, ExpectedHash} =
        case couch_util:get_value(<<"password_scheme">>, UserProps, <<"simple">>) of
        <<"simple">> ->
            {couch_passwords:simple(Pass, UserSalt),
            couch_util:get_value(<<"password_sha">>, UserProps, nil)};
        <<"pbkdf2">> ->
            Iterations = couch_util:get_value(<<"iterations">>, UserProps, 10000),
            {couch_passwords:pbkdf2(Pass, UserSalt, Iterations),
             couch_util:get_value(<<"derived_key">>, UserProps, nil)}
    end,
    couch_passwords:verify(PasswordHash, ExpectedHash).

auth_name(String) when is_list(String) ->
    [_,_,_,_,_,Name|_] = re:split(String, "[\\W_]", [{return, list}]),
    list_to_binary(Name).

make_cookie_time() ->
    {NowMS, NowS, _} = os:timestamp(),
    NowMS * 1000000 + NowS.

cookie_scheme(#httpd{mochi_req=MochiReq}) ->
    [{http_only, true}] ++
    case MochiReq:get(scheme) of
        http -> [];
        https -> [{secure, true}]
    end.

max_age() ->
    case barrel_config:get("couch_httpd_auth", "allow_persistent_cookies", "false") of
        "false" ->
            [];
        "true" ->
            Timeout = barrel_config:get_integer("couch_httpd_auth", "timeout", 600),
            [{max_age, Timeout}]
    end.
