%% Copyright (c) 2016, Benoit Chesneau
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

-module(barrel_legacy_handler).
-author("Benoit Chesneau").

%% API
-export([init/3, loop/1]).
-export([options/0]).

-export([apply_cors_headers/2]).
-export([host/1]).

-include_lib("couch_db.hrl").

init(_, _, _) ->  {upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
  Opts = Req:get(opts),
  Req2 = case proplists:get_value(prefix, Opts) of
           undefined ->
             Req;
           Prefix ->
             Path = Req:get(raw_path),
             case re:split(Path, Prefix, [{return, list}]) of
               ["", NewPath] ->
                 NewPath1 = case NewPath of
                              "" -> "/";
                              _ -> NewPath
                            end,
                 mochiweb_request:new(Req:get(socket),
                   Req:get(method),
                   NewPath1,
                   Req:get(version),
                   Req:get(headers));
               _Else ->
                 Req
             end
         end,

  DefaultFun = proplists:get_value(default_fun, Opts),
  UrlHandlers = proplists:get_value(url_handlers, Opts),
  DbUrlHandlers = proplists:get_value(db_url_handlers, Opts),
  DesignUrlHandlers = proplists:get_value(design_url_handlers, Opts),

  couch_httpd:handle_request(Req2, DefaultFun, UrlHandlers, DbUrlHandlers,
    DesignUrlHandlers).

options() ->
  DefaultSpec = "{couch_httpd_db, handle_request}",
  DefaultFun = couch_httpd:make_arity_1_fun(
    barrel_config:get("httpd", "default_handler", DefaultSpec)
  ),

  UrlHandlersList = lists:map(
    fun({UrlKey, SpecStr}) ->
      {list_to_binary(UrlKey), couch_httpd:make_arity_1_fun(SpecStr)}
    end, barrel_config:get("httpd_global_handlers")),

  DbUrlHandlersList = lists:map(
    fun({UrlKey, SpecStr}) ->
      {list_to_binary(UrlKey), couch_httpd:make_arity_2_fun(SpecStr)}
    end, barrel_config:get("httpd_db_handlers")),

  DesignUrlHandlersList = lists:map(
    fun({UrlKey, SpecStr}) ->
      {list_to_binary(UrlKey), couch_httpd:make_arity_3_fun(SpecStr)}
    end, barrel_config:get("httpd_design_handlers")),

  UrlHandlers = dict:from_list(UrlHandlersList),
  DbUrlHandlers = dict:from_list(DbUrlHandlersList),
  DesignUrlHandlers = dict:from_list(DesignUrlHandlersList),

  set_auth_handlers(),

  % add barrel log event handler
  lager_handler_watcher:start(lager_event, barrel_log_event_handler, []),

  [{url_handlers, UrlHandlers}, {db_url_handlers, DbUrlHandlers},
    {design_url_handlers, DesignUrlHandlers}, {default_fun, DefaultFun},
    {loop, {?MODULE, loop}}].


set_auth_handlers() ->
  AuthenticationSrcs = couch_httpd:make_fun_spec_strs(
    barrel_config:get("httpd", "authentication_handlers", "")),
  AuthHandlers = lists:map(
    fun(A) -> {couch_httpd:make_arity_1_fun(A), list_to_binary(A)} end, AuthenticationSrcs),
  ok = application:set_env(couch_httpd, auth_handlers, AuthHandlers).

apply_cors_headers(#httpd{mochi_req=MochiReq}, Headers) ->
  apply_cors_headers(MochiReq, Headers);
apply_cors_headers(Req, Headers) ->
  case barrel_cors_config:enabled() of
    true ->
      Origin = Req:get_header_value("Origin"),
      Host = host(Req),
      AllowAll = barrel_cors_config:allow_all(),
      case Origin of
        undefined -> Headers;
        _ when AllowAll =:= true ->
          do_apply_cors_headers(barrel_lib:to_binary(Origin), Host, Headers);
        _ ->
          Origin1 = barrel_lib:to_binary(Origin),
          case barrel_cors_middleware:match_origin(barrel_cors_config:allow_origins(), Origin1) of
            true ->
              do_apply_cors_headers(Origin1, Host, Headers);
            false ->
              Headers
          end
      end;
    false ->
      Headers
  end.

do_apply_cors_headers(Origin, Host, Headers0) ->
  Headers = [{"Access-Control-Allow-Origin", binary_to_list(Origin)} | Headers0],
  case barrel_cors_middleware:allow_credentials(Origin, Host) of
    true -> [{"Access-Control-Allow-Credentials", "true"} | Headers];
    false -> Headers
  end.


host(Req) ->
  XHost = barrel_config:get("api", "x_forwarded_host", "X-Forwarded-Host"),
  case Req:get_header_value(XHost) of
    undefined ->
      case Req:get_header_value("Host") of
        undefined -> <<>>;
        Value -> barrel_lib:to_binary(Value)
      end;
    Value -> barrel_lib:to_binary(Value)
  end.