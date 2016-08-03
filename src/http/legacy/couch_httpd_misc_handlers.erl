%% Copyright 2016, Benoit Chesneau
%% Copyright 2009-2014 The Apache Software Foundation
%%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_misc_handlers).

-export([handle_welcome_req/1, handle_all_dbs_req/1,
         handle_uuids_req/1,  handle_task_status_req/1,  handle_up_req/1]).

-export([increment_update_seq_req/2]).

-include("db.hrl").


-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, send_error/4]).

-define(DEFAULT_LIMIT, 16#10000000).

% httpd global handlers

handle_welcome_req(#httpd{method='GET'}=Req) ->
    send_json(Req, maps:from_list(
        [{name, barrel}, {uuid, barrel_server:node_id()}] ++
        case barrel_config:get("vendor") of
            [] -> [];
            Properties ->
                [{vendor, maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- Properties])
                }]
        end
    ));
handle_welcome_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_all_dbs_req(#httpd{method='GET'}=Req) ->
    Limit0 = barrel_lib:to_integer(couch_httpd:qs_value(Req, "limit",
                                                        ?DEFAULT_LIMIT)),
    Skip0 = barrel_lib:to_integer(couch_httpd:qs_value(Req, "skip", -1)),
    {ok, {DbNames, _, _}} = barrel_server:all_databases(fun all_dbs_fun/2, {[], Skip0, Limit0}),
    send_json(Req, lists:usort(DbNames));
handle_all_dbs_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").


all_dbs_fun(_DbName, {Acc, Skip, 0}) ->
    {stop, {Acc, Skip, 0}};
all_dbs_fun(DbName, {Acc, 0, Limit}) ->
    {ok, {[DbName | Acc], 0, Limit - 1}};
all_dbs_fun(_DbName, {Acc, Skip, Limit}) when Skip > 0 ->
    {ok, {Acc, Skip - 1, Limit}};
all_dbs_fun (DbName, {Acc, Skip, Limit}) ->
    {ok, {[DbName | Acc], Skip, Limit - 1}}.

handle_task_status_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    % convert the list of prop lists to a list of json objects
    send_json(Req, [maps:from_list(Props) || Props <- barrel_task_status:all()]);
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_uuids_req(#httpd{method='GET'}=Req) ->
    Count = list_to_integer(couch_httpd:qs_value(Req, "count", "1")),
    UUIDs = [barrel_uuids:new() || _ <- lists:seq(1, Count)],
    Etag = couch_httpd:make_etag(UUIDs),
    couch_httpd:etag_respond(Req, Etag, fun() ->
        CacheBustingHeaders = [
            {"Date", barrel_lib:rfc1123_date()},
            {"Cache-Control", "no-cache"},
            % Past date, ON PURPOSE!
            {"Expires", "Fri, 01 Jan 1990 00:00:00 GMT"},
            {"Pragma", "no-cache"},
            {"ETag", Etag}
        ],
        send_json(Req, 200, CacheBustingHeaders, #{<<"uuids">> => UUIDs})
    end);
handle_uuids_req(Req) ->
    send_method_not_allowed(Req, "GET").

% httpd db handlers

increment_update_seq_req(#httpd{method='POST'}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {ok, NewSeq} = couch_db:increment_update_seq(Db),
    send_json(Req, {[{ok, true},
        {update_seq, NewSeq}
    ]});
increment_update_seq_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

% httpd log handlers

handle_up_req(#httpd{method='GET'} = Req) ->
    case barrel_config:get("barrel", "maintenance_mode") of
        "true" ->
            couch_httpd:send_json(Req, 404, #{status => maintenance_mode});
        _ ->
            couch_httpd:send_json(Req, 200, #{status => ok})
    end;
handle_up_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET,HEAD").
