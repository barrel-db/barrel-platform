%% Copyright (c) 2016, Benoit Chesneau
%% Copyright 2009-2014 The Apache Software Foundation
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
%%

%% Created by benoitc on 10/06/16.

-module(barrel_cors_middleware).
-behaviour(cowboy_middleware).
-author("Benoit Chesneau").

%% API
-export([execute/2]).
-export([init_config/0]).

-define(MAX_AGE, 1728000). %% 20 days
-define(HEADERS_ALLOWED,  [<<"Accept">>, <<"Accept-Language">>, <<"Content-Type">>,
  <<"Expires">>, <<"Last-Modified">>, <<"Pragma">>, <<"Origin">>, <<"Content-Length">>,
  <<"If-Match">>, <<"Destination">>, <<"X-Requested-With">>, <<"X-Http-Method-Override">>,
  <<"Content-Range">>]).

-define(METHODS_ALLOWED, [<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>, <<"DELETE">>,
  <<"OPTIONS">>, <<"COPY">>, <<"PATCH">>]).


execute(Req, Env) ->
  IsEnabled = barrel_cors_config:enabled(),
  case cowboy_req:method(Req) of
    {<<"OPTIONS">>, Req2} when IsEnabled =:= true ->
      {Origin, Req3} = cowboy_req:header(<<"origin">>, Req2),
      handle_preflight_request(Origin, Req3, Env);
    {_, Req2} when IsEnabled =:= true ->
      {Origin, Req2} = cowboy_req:header(<<"origin">>, Req),
      apply_cors_headers(Origin, Req2, Env);
    {_, Req2} ->
      {ok, Req2, Env}
  end.

init_config() ->
  Enabled = barrel_config:get_boolean("cors", "enabled", false) =:= true,
  AllowMethods = items_to_binary(barrel_config:get_list("cors", "allow_methods", ?METHODS_ALLOWED)),
  AllowHeaders = items_to_binary(barrel_config:get_list("cors", "allow_headers", ?HEADERS_ALLOWED)),
  AllowHeadersLowercase = [cowboy_bstr:to_lower(H) || H <- AllowHeaders] ,
  MaxAge = barrel_config:get_integer("cors", "max_age", ?MAX_AGE),
  AllowCredentials = barrel_config:get_boolean("cors", "allow_credentials", false),
  AllowOrigins = items_to_binary(barrel_config:get_list("cors", "allow_origins", [])),
  AllowAll = lists:member(<<"*">>, AllowOrigins),
  Cfg = [{enabled, Enabled}, {allow_methods, AllowMethods}, {allow_headers, AllowHeaders},
    {allow_headers_lowercase, AllowHeadersLowercase}, {max_age, MaxAge},
    {allow_credentials, AllowCredentials}, {allow_origins, AllowOrigins}, {allow_all, AllowAll}],

  barrel_lib:load_config(barrel_cors_config, Cfg).

handle_preflight_request(undefined, Req, Env) -> {ok, Req, Env};
handle_preflight_request(Origin, Req, Env) ->
  case barrel_cors_config:allow_all() of
    true ->
      send_preflight_response(Origin, Req, Env);
    false ->
      case match_origin(barrel_cors_config:allow_origins(), Origin) of
        true -> send_preflight_response(Origin, Req, Env);
        false -> {ok, Req, Env}
      end
  end.

send_preflight_response(Origin, Req, Env) ->
  Host = barrel_api_http:host(Req),
  PreflightHeaders0 = add_credentials(Origin, Host,
    [{<<"Access-Control-Allow-Origin">>, Origin}
      ,{<<"Access-Control-Max-Age">>, integer_to_binary(barrel_cors_config:max_age())}
      ,{<<"Access-Control-Allow-Methods">>, barrel_lib:join(barrel_cors_config:allow_methods(), <<",">>)}]),

  case check_req_method(Req) of
    true ->
      case check_req_headers(Req, PreflightHeaders0) of
        false ->
          {ok, Req, Env};
        PreflightHeaders ->
          {ok, Req2} =  cowboy_req:reply(200, PreflightHeaders, Req),
          {halt, Req2}
       end;
    false ->
      {ok, Req, Env}
  end.

add_credentials(Origin, Host, Headers) ->
  case allow_credentials(Origin, Host) of
    true -> [{<<"Access-Control-Allow-Credentials">>, <<"true">>} | Headers];
    false -> Headers
  end.

check_req_method(Req) ->
  case cowboy_req:header(<<"access-control-request-method">>, Req) of
    {undefined, _} -> true;
    {Method, _} ->
      lists:member(Method, barrel_cors_config:allow_methods())
  end.

check_req_headers(Req, Headers) ->
  case cowboy_req:header(<<"access-control-request-headers">>, Req) of
    {undefined, _} -> Headers;
    {AccessHeaders, _} ->
      LAccessHeaders = [cowboy_bstr:to_lower(H) || H <- AccessHeaders],
      case LAccessHeaders -- barrel_cors_config:allow_headers_lowercase() of
        [] -> [{<<"Access-Control-Allow-Headers">>, AccessHeaders} | Headers];
        _ -> false
      end
  end.

apply_cors_headers(undefined, Req, Env) ->
  {ok, Req, Env};
apply_cors_headers(Origin, Req, Env) ->
  Host = barrel_api_http:host(Req),
  case barrel_cors_config:allow_all() of
    true -> do_apply_cors_headers(Origin, Host, Req, Env);
    false ->
      case match_origin(barrel_cors_config:allow_origins(), Origin) of
        true -> do_apply_cors_headers(Origin, Host, Req, Env);
        false -> {ok, Req, Env}
      end
  end.

do_apply_cors_headers(Origin, Host, Req, Env) ->
  Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, Req),
  case allow_credentials(Origin, Host) of
    true ->
      Req3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Credentials">>, <<"true">>, Req2),
      {ok, Req3, Env};
    false ->
      {ok, Req2, Env}
  end.

match_origin([], _) -> false;
match_origin([Pattern | Rest], Origin) ->
  case binary:match(Origin, Pattern)  of
    nomatch -> match_origin(Rest, origin);
    _ -> true
  end.

allow_credentials(<<"*">>, _) -> false;
allow_credentials(H, H) -> barrel_cors_config:allow_credentials().

items_to_binary(L) -> [barrel_lib:to_binary(V) || V <- L].

