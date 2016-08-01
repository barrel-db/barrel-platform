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

-include("db.hrl").

init(_, Req, _) ->
  {UserCtx, _} = cowboy_req:meta(user_ctx, Req, barrel_lib:userctx()),
  put(user_ctx, UserCtx),
  {upgrade, protocol, mochicow_upgrade}.

loop(Req) ->
  Opts = Req:get(opts),
  UserCtx = get(user_ctx),
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
    DesignUrlHandlers, UserCtx).

options() ->

  DefaultFun = fun couch_httpd_db:handle_request/1,

  UrlHandlersList = [
    {<<"/">>,  fun couch_httpd_misc_handlers:handle_welcome_req/1},
    {<<"_all_dbs">>, fun couch_httpd_misc_handlers:handle_all_dbs_req/1},
    {<<"_active_tasks">>, fun couch_httpd_misc_handlers:handle_task_status_req/1},
    {<<"_replicate">>, fun couch_replicator_httpd:handle_req/1},
    {<<"_uuids">>, fun couch_httpd_misc_handlers:handle_uuids_req/1},
    {<<"_db_updates">>, fun couch_dbupdates_httpd:handle_req/1}
  ],

  DbUrlHandlersList = [
    {<<"_all_docs">>, fun couch_httpd_all_docs:handle_req/2},
    {<<"_changes">>, fun couch_httpd_changes:handle_changes_req/2},
    {<<"_compact">>, fun couch_httpd_db:handle_compact_req/2},
    {<<"_design">>, fun couch_httpd_db:handle_design_req/2},
    {<<"_temp_view">>, fun couch_mrview_http:handle_temp_view_req/2},
    {<<"_view_cleanup">>, fun couch_mrview_http:handle_cleanup_req/2},
    {<<"_random_doc">>, fun couch_randomdoc_httpd:handle_req/2},
    {<<"_bulk_get">>, fun couch_httpd_bulk_get:handle_req/2}
  ],

  DesignUrlHandlersList = [
    {<<"_compact">>, fun couch_mrview_http:handle_compact_req/3},
    {<<"_info">>, fun couch_mrview_http:handle_info_req/3},
    {<<"_list">>, fun couch_mrview_show:handle_view_list_req/3},
    {<<"_show">>, fun couch_mrview_show:handle_doc_show_req/3},
    {<<"_update">>, fun couch_mrview_show:handle_doc_update_req/3},
    {<<"_view">>, fun couch_mrview_http:handle_view_req/3},
    {<<"_reindex">>, fun couch_mrview_http:handle_reindex_req/3}
  ],

  UrlHandlers = dict:from_list(UrlHandlersList),
  DbUrlHandlers = dict:from_list(DbUrlHandlersList),
  DesignUrlHandlers = dict:from_list(DesignUrlHandlersList),

  [{url_handlers, UrlHandlers}, {db_url_handlers, DbUrlHandlers},
    {design_url_handlers, DesignUrlHandlers}, {default_fun, DefaultFun},
    {loop, {?MODULE, loop}}].

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
  XHost = barrrel_server:get_env(x_forwarded_host),
  case Req:get_header_value(XHost) of
    undefined ->
      case Req:get_header_value("Host") of
        undefined -> <<>>;
        Value -> barrel_lib:to_binary(Value)
      end;
    Value -> barrel_lib:to_binary(Value)
  end.
