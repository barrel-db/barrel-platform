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

-export([handle_welcome_req/1,handle_favicon_req/2,handle_utils_dir_req/2,
         handle_all_dbs_req/1,handle_restart_req/1,
         handle_uuids_req/1,handle_config_req/1,handle_log_req/1,
         handle_task_status_req/1, handle_file_req/2,
         handle_up_req/1]).

-export([increment_update_seq_req/2]).

-include_lib("couch/include/couch_db.hrl").


-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, send_error/4]).

-define(DEFAULT_LIMIT, 16#10000000).

% httpd global handlers

handle_welcome_req(#httpd{method='GET'}=Req) ->
    send_json(Req, {[
        {name, barrel},
        {uuid, couch_server:get_uuid()}
        ] ++ case barrel_config:get("vendor") of
            [] -> [];
            Properties ->
                [{vendor, {[{list_to_binary(K), ?l2b(V)} || {K, V} <- Properties]}}]
        end
    });
handle_welcome_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_favicon_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    {{Year,Month,Day},Time} = erlang:universaltime(),
    OneYearFromNow = {{Year+1,Month,Day},Time},
    CachingHeaders = [
        %favicon should expire a year from now
        {"Cache-Control", "public, max-age=31536000"},
        {"Expires", couch_util:rfc1123_date(OneYearFromNow)}
    ],
    couch_httpd:serve_file(Req, "favicon.ico", DocumentRoot, CachingHeaders);

handle_favicon_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_file_req(#httpd{method='GET'}=Req, Document) ->
    couch_httpd:serve_file(Req, filename:basename(Document), filename:dirname(Document));

handle_file_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_utils_dir_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    "/" ++ UrlPath = couch_httpd:path(Req),
    case couch_httpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        % GET /_utils/path or GET /_utils/
        CachingHeaders =
                [{"Cache-Control", "private, must-revalidate"}],
        couch_httpd:serve_file(Req, RelativePath, DocumentRoot, CachingHeaders);
    {_ActionKey, "", _RelativePath} ->
        % GET /_utils
        RedirectPath = couch_httpd:path(Req) ++ "/",
        couch_httpd:send_redirect(Req, RedirectPath)
    end;
handle_utils_dir_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_all_dbs_req(#httpd{method='GET'}=Req) ->
    Limit0 = couch_util:to_integer(couch_httpd:qs_value(Req, "limit",
                                                        ?DEFAULT_LIMIT)),
    Skip0 = couch_util:to_integer(couch_httpd:qs_value(Req, "skip", -1)),
    {ok, {DbNames, _, _}} = couch_server:all_databases(fun all_dbs_fun/2,
                                                       {[], Skip0, Limit0}),
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
    send_json(Req, [{Props} || Props <- couch_task_status:all()]);
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").


handle_restart_req(#httpd{method='POST'}=Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = couch_httpd:verify_is_server_admin(Req),
    Result = send_json(Req, 202, {[{ok, true}]}),
    couch_sup:restart_core_server(),
    Result;
handle_restart_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_uuids_req(#httpd{method='GET'}=Req) ->
    Count = list_to_integer(couch_httpd:qs_value(Req, "count", "1")),
    UUIDs = [barrel_uuids:new() || _ <- lists:seq(1, Count)],
    Etag = couch_httpd:make_etag(UUIDs),
    couch_httpd:etag_respond(Req, Etag, fun() ->
        CacheBustingHeaders = [
            {"Date", couch_util:rfc1123_date()},
            {"Cache-Control", "no-cache"},
            % Past date, ON PURPOSE!
            {"Expires", "Fri, 01 Jan 1990 00:00:00 GMT"},
            {"Pragma", "no-cache"},
            {"ETag", Etag}
        ],
        send_json(Req, 200, CacheBustingHeaders, {[{<<"uuids">>, UUIDs}]})
    end);
handle_uuids_req(Req) ->
    send_method_not_allowed(Req, "GET").


% Config request handler


% GET /_config/
% GET /_config
handle_config_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    KVs = lists:foldl(fun({Section, Values0}, Acc) ->
                Values = [{list_to_binary(K), ?l2b(V)} || {K, V} <- Values0],
                [{list_to_binary(Section), {Values}} | Acc]
    end, [], barrel_config:all()),
    send_json(Req, 200, {KVs});
% GET /_config/Section
handle_config_req(#httpd{method='GET', path_parts=[_,Section]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    KVs = [{list_to_binary(K), ?l2b(V)} || {K, V} <- barrel_config:get(binary_to_list(Section))],
    send_json(Req, 200, {KVs});
% GET /_config/Section/Key
handle_config_req(#httpd{method='GET', path_parts=[_, Section, Key]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    case barrel_config:get(binary_to_list(Section), ?b2l(Key), null) of
    null ->
        throw({not_found, unknown_config_value});
    Value ->
        send_json(Req, 200, list_to_binary(Value))
    end;
% PUT or DELETE /_config/Section/Key
handle_config_req(#httpd{method=Method, path_parts=[_, Section, Key]}=Req)
      when (Method == 'PUT') or (Method == 'DELETE') ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Persist = couch_httpd:header_value(Req, "X-Couch-Persist") /= "false",
    case barrel_config:get("httpd", "config_whitelist", null) of
        null ->
            % No whitelist; allow all changes.
            handle_approved_config_req(Req, Persist);
        WhitelistValue ->
            % Provide a failsafe to protect against inadvertently locking
            % onesself out of the config by supplying a syntactically-incorrect
            % Erlang term. To intentionally lock down the whitelist, supply a
            % well-formed list which does not include the whitelist config
            % variable itself.
            FallbackWhitelist = [{"httpd", "config_whitelist"}],

            Whitelist = case couch_util:parse_term(WhitelistValue) of
                {ok, Value} when is_list(Value) ->
                    Value;
                {ok, _NonListValue} ->
                    FallbackWhitelist;
                {error, _} ->
                    [{WhitelistSection, WhitelistKey}] = FallbackWhitelist,
                    barrel_log:error("Only whitelisting ~s/~s due to error parsing: ~p",
                               [WhitelistSection, WhitelistKey, WhitelistValue]),
                    FallbackWhitelist
            end,

            IsRequestedKeyVal = fun(Element) ->
                case Element of
                    {A, B} ->
                        % For readability, tuples may be used instead of binaries
                        % in the whitelist.
                        case {couch_util:to_binary(A), couch_util:to_binary(B)} of
                            {Section, Key} ->
                                true;
                            {Section, <<"*">>} ->
                                true;
                            _Else ->
                                false
                        end;
                    _Else ->
                        false
                end
            end,

            case lists:any(IsRequestedKeyVal, Whitelist) of
                true ->
                    % Allow modifying this whitelisted variable.
                    handle_approved_config_req(Req, Persist);
                _NotWhitelisted ->
                    % Disallow modifying this non-whitelisted variable.
                    send_error(Req, 400, <<"modification_not_allowed">>,
                               list_to_binary("This config variable is read-only"))
            end
    end;
handle_config_req(Req) ->
    send_method_not_allowed(Req, "GET,PUT,DELETE").

% PUT /_config/Section/Key
% "value"
handle_approved_config_req(Req, Persist) ->
    Query = couch_httpd:qs(Req),
    UseRawValue = case lists:keyfind("raw", 1, Query) of
    false            -> false; % Not specified
    {"raw", ""}      -> false; % Specified with no value, i.e. "?raw" and "?raw="
    {"raw", "false"} -> false;
    {"raw", "true"}  -> true;
    {"raw", InvalidValue} -> InvalidValue
    end,
    handle_approved_config_req(Req, Persist, UseRawValue).

handle_approved_config_req(#httpd{method='PUT', path_parts=[_, Section0, Key0]}=Req,
                           Persist, UseRawValue)
        when UseRawValue =:= false orelse UseRawValue =:= true ->
    Section = binary_to_list(Section0),
    Key = binary_to_list(Key0),
    RawValue = couch_httpd:json_body(Req),
    Value = case UseRawValue of
    true ->
        % Client requests no change to the provided value.
        RawValue;
    false ->
        % Pre-process the value as necessary.
        case Section0 of
        <<"admins">> ->
            couch_passwords:hash_admin_password(RawValue);
        _ ->
            RawValue
        end
    end,

    OldValue = barrel_config:get_binary(Section, Key, <<"">>),
    case barrel_config:set(Section, Key, Value, Persist) of
    ok ->
        send_json(Req, 200, OldValue);
    Error ->
        throw(Error)
    end;

handle_approved_config_req(#httpd{method='PUT'}=Req, _Persist, UseRawValue) ->
    Err = io_lib:format("Bad value for 'raw' option: ~s", [UseRawValue]),
    send_json(Req, 400, {[{error, list_to_binary(Err)}]});

% DELETE /_config/Section/Key
handle_approved_config_req(#httpd{method='DELETE',path_parts=[_,Section0,Key0]}=Req,
                           Persist, _UseRawValue) ->
    Section = binary_to_list(Section0),
    Key = binary_to_list(Key0),
    case barrel_config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    OldValue ->
        barrel_config:del(Section, Key, Persist),
        send_json(Req, 200, list_to_binary(OldValue))
    end.


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

handle_log_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Bytes = list_to_integer(couch_httpd:qs_value(Req, "bytes", "1000")),
    Offset = list_to_integer(couch_httpd:qs_value(Req, "offset", "0")),
    Formatted = couch_httpd:qs_value(Req, "formatted", "false"),
    RawChunk = barrel_log:read(Bytes, Offset),
    case Formatted of
        "true" ->
            EndOfLine = string:chr(RawChunk, $\n),
            Chunk = string:substr(RawChunk, EndOfLine + 1);
        _ ->
            Chunk = RawChunk
    end,

    {ok, Resp} = start_chunked_response(Req, 200, [
        % send a plaintext response
        {"Content-Type", "text/plain; charset=utf-8"},
        {"Content-Length", integer_to_list(length(Chunk))}
    ]),
    send_chunk(Resp, Chunk),
    last_chunk(Resp);
handle_log_req(#httpd{method='POST'}=Req) ->
    {PostBody} = couch_httpd:json_body_obj(Req),
    Level = couch_util:get_value(<<"level">>, PostBody),
    Message = binary_to_list(couch_util:get_value(<<"message">>, PostBody)),
    case Level of
    <<"debug">> ->
        barrel_log:debug(Message, []),
        send_json(Req, 200, {[{ok, true}]});
    <<"info">> ->
        barrel_log:info(Message, []),
        send_json(Req, 200, {[{ok, true}]});
    <<"error">> ->
        barrel_log:error(Message, []),
        send_json(Req, 200, {[{ok, true}]});
    _ ->
        send_json(Req, 400, {[{error, list_to_binary(io_lib:format("Unrecognized log level '~s'", [Level]))}]})
    end;
handle_log_req(Req) ->
    send_method_not_allowed(Req, "GET,POST").

handle_up_req(#httpd{method='GET'} = Req) ->
    case barrel_config:get("couchdb", "maintenance_mode") of
        "true" ->
            couch_httpd:send_json(Req, 404, {[{status, maintenance_mode}]});
        _ ->
            couch_httpd:send_json(Req, 200, {[{status, ok}]})
    end;
handle_up_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET,HEAD").
