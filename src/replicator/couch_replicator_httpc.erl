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

-module(couch_replicator_httpc).

-include_lib("couch_db.hrl").
-include("couch_replicator_api_wrap.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").

-define(STREAM_STATUS, ibrowse_stream_status).

-export([setup/1]).
-export([send_req/3]).
-export([full_url/2]).

-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).
-define(MAX_WAIT, 5 * 60 * 1000).
-define(MAX_DISCARDED_MSGS, 16).


setup(#httpdb{httpc_pool = nil, url = Url, http_connections = MaxConns} = Db) ->
    {ok, Pid} = couch_replicator_httpc_pool:start_link(Url, [{max_connections, MaxConns}]),
    {ok, Db#httpdb{httpc_pool = Pid}}.


send_req(HttpDb, Params1, Callback) ->
    put(?STREAM_STATUS, init),
    Params2 = ?replace(Params1, qs,
                       [{K, binary_to_list(to_param(V))} || {K, V} <- proplists:get_value(qs, Params1, [])]),
    Params = ?replace(Params2, ibrowse_options,
        lists:keysort(1, proplists:get_value(ibrowse_options, Params2, []))),
    {Worker, Response} = send_ibrowse_req(HttpDb, Params),
    Ret = try
        process_response(Response, Worker, HttpDb, Params, Callback)
    catch
        throw:{retry, NewHttpDb0, NewParams0} ->
            {retry, NewHttpDb0, NewParams0}
    after
        release_worker(Worker, HttpDb),
        clean_mailbox(Response)
    end,
    % This is necessary to keep this tail-recursive. Calling
    % send_req in the catch clause would turn it into a body
    % recursive call accidentally.
    case Ret of
        {retry, #httpdb{}=NewHttpDb, NewParams} ->
            send_req(NewHttpDb, NewParams, Callback);
        _ ->
            Ret
    end.


to_param(false) ->
    <<"false">>;
to_param(true) ->
    <<"true">>;
to_param(V) ->
    iolist_to_binary(V).


send_ibrowse_req(#httpdb{headers = BaseHeaders} = HttpDb, Params) ->
    Method = proplists:get_value(method, Params, get),
    UserHeaders = lists:keysort(1, proplists:get_value(headers, Params, [])),
    Headers1 = lists:ukeymerge(1, UserHeaders, BaseHeaders),
    Headers2 = oauth_header(HttpDb, Params) ++ Headers1,
    Url = full_url(HttpDb, Params),
    Body = proplists:get_value(body, Params, []),
    IsChanges = proplists:get_value(path, Params) == "_changes",
    Timeout = case IsChanges of
                true -> infinity;
                _ ->
                  case barrel_config:get_env(request_timeout) of
                    "infinity" -> infinity;
                    Ms -> list_to_integer(Ms)
                  end
              end,
    {ok, Worker} = couch_replicator_httpc_pool:get_worker(HttpDb#httpdb.httpc_pool),
    IbrowseOptions = [
        {response_format, binary}, {inactivity_timeout, HttpDb#httpdb.timeout} |
        lists:ukeymerge(1, proplists:get_value(ibrowse_options, Params, []),
            HttpDb#httpdb.ibrowse_options)
    ],
    Response = ibrowse:send_req_direct(Worker, Url, Headers2, Method,
         Body, IbrowseOptions, Timeout),
    {Worker, Response}.


process_response({error, req_timedout}, _Worker, HttpDb, Params, _Cb) ->
    throw({retry, HttpDb, Params});

process_response({error, connection_closing}, Worker, HttpDb, Params, _Cb) ->
    MRef = erlang:monitor(process, Worker),
    ibrowse_http_client:stop(Worker),
    receive
      {'DOWN', MRef, _, _} -> ok
    end,
    couch_replicator_httpc:release_worker_sync(HttpDb#httpdb.httpc_pool, Worker),
    throw({retry, HttpDb, Params});

process_response({error, sel_conn_closed}, _Worker, HttpDb, Params,  _Cb) ->
    throw({retry, HttpDb, Params});

process_response({error, {'EXIT', {normal, _}}}, _Worker, HttpDb,
                 Params, _Cb) ->
    % ibrowse worker terminated because remote peer closed the socket
    % -> not an error
    throw({retry, HttpDb, Params});

process_response({ibrowse_req_id, ReqId}, Worker, HttpDb, Params, Callback) ->
    process_stream_response(ReqId, Worker, HttpDb, Params, Callback);

process_response({ok, Code, Headers, Body}, Worker, HttpDb, Params, Callback) ->
    case list_to_integer(Code) of
    Ok when (Ok >= 200 andalso Ok < 300) ; (Ok >= 400 andalso Ok < 500) ->
        EJson = case Body of
        <<>> ->
            null;
        Json ->
            ?JSON_DECODE(Json)
        end,
        Callback(Ok, Headers, EJson);
    R when R =:= 301 ; R =:= 302 ; R =:= 303 ->
        do_redirect(Worker, R, Headers, HttpDb, Params, Callback);
    Error ->
        maybe_retry({code, Error}, Worker, HttpDb, Params)
    end;

process_response(Error, Worker, HttpDb, Params, _Cb) ->
    maybe_retry(Error, Worker, HttpDb, Params).


process_stream_response(ReqId, Worker, HttpDb, Params, Callback) ->
    receive
    {ibrowse_async_headers, ReqId, Code, Headers} ->
        case list_to_integer(Code) of
        Ok when (Ok >= 200 andalso Ok < 300) ; (Ok >= 400 andalso Ok < 500) ->
            StreamDataFun = fun() ->
                stream_data_self(HttpDb, Params, Worker, ReqId, Callback)
            end,
            put(?STREAM_STATUS, {streaming, Worker}),
            ibrowse:stream_next(ReqId),
            try
                Ret = Callback(Ok, Headers, StreamDataFun),
                Ret
            catch
                throw:{maybe_retry_req, Err} ->
                    maybe_retry(Err, Worker, HttpDb, Params)
            end;
        R when R =:= 301 ; R =:= 302 ; R =:= 303 ->
            do_redirect(Worker, R, Headers, HttpDb, Params, Callback);
        Error ->
            report_error(Worker, HttpDb, Params, {code, Error})
        end;
    {ibrowse_async_response, ReqId, {error, _} = Error} ->
        maybe_retry(Error, Worker, HttpDb, Params)
    after HttpDb#httpdb.timeout + 500 ->
        % Note: ibrowse should always reply with timeouts, but this doesn't
        % seem to be always true when there's a very high rate of requests
        % and many open connections.
        maybe_retry(timeout, Worker, HttpDb, Params)
    end.


% Only streaming HTTP requests send messages back from
% the ibrowse worker process. We can detect that based
% on the ibrowse_req_id format. This just drops all
% messages for the given ReqId on the floor since we're
% no longer in the HTTP request.

clean_mailbox(ReqId) ->
  clean_mailbox(ReqId, get(?STREAM_STATUS), ?MAX_DISCARDED_MSGS).

clean_mailbox(_ReqId, Status, 0) ->
  case Status of
    {streaming, Worker} -> exit(Worker, {timeout, ibrowse_stream_cleanup});
    _ -> ok
  end;

clean_mailbox({ibrowse_req_id, ReqId}, Status, N) ->
  case Status of
    {streaming, Worker} ->
      ibrowse:stream_next(ReqId),
      receive
        {ibrowse_async_response, ReqId, _} ->
          clean_mailbox({ibrowse_req_id, ReqId}, Status, N - 1);
        {ibrowse_async_response_end, ReqId} ->
          put(?STREAM_STATUS, ended),
          ok
      after 30000 ->
        exit(Worker, {timeout, ibrowse_stream_cleanup}),
        exit({timeout, ibrowse_stream_cleanup})
      end;
    _ when Status =:= init; Status =:= ended ->
      receive
        {ibrowse_async_response, ReqId, _} ->
          clean_mailbox({ibrowse_req_id, ReqId}, Status, N-1);
        {ibrowse_async_response_end, ReqId} ->
          clean_mailbox({ibrowse_req_id, ReqId}, Status, N-1)
      after 0 ->
        ok
      end
  end;
clean_mailbox(_, _, _) ->
    ok.


release_worker(Worker, #httpdb{httpc_pool = Pool}) ->
  ok = couch_replicator_httpc_pool:release_worker(Pool, Worker).


maybe_retry(Error, Worker, #httpdb{retries = 0} = HttpDb, Params) ->
    report_error(Worker, HttpDb, Params, {error, Error});

maybe_retry(Error, _Worker, #httpdb{retries = Retries, wait = Wait} = HttpDb,
    Params) ->
    Method = string:to_upper(atom_to_list(proplists:get_value(method, Params, get))),
    Url = barrel_lib:url_strip_password(full_url(HttpDb, Params)),
    lager:info("Retrying ~s request to ~s in ~p seconds due to error ~s",
        [Method, Url, Wait / 1000, error_cause(Error)]),
    ok = timer:sleep(Wait),
    Wait2 = erlang:min(Wait * 2, ?MAX_WAIT),
    NewHttpDb = HttpDb#httpdb{retries = Retries - 1, wait = Wait2},
    throw({retry, NewHttpDb, Params}).

report_error(_Worker, HttpDb, Params, Error) ->
    Method = string:to_upper(atom_to_list(proplists:get_value(method, Params, get))),
    Url = barrel_lib:url_strip_password(full_url(HttpDb, Params)),
    do_report_error(Url, Method, Error),
    exit({http_request_failed, Method, Url, Error}).


do_report_error(Url, Method, {code, Code}) ->
    lager:error("Replicator, request ~s to ~p failed. The received HTTP error code is ~p", [Method, Url, Code]);

do_report_error(FullUrl, Method, Error) ->
    lager:error("Replicator, request ~s to ~p failed due to error ~s",
        [Method, FullUrl, error_cause(Error)]).


error_cause({error, Cause}) ->
    lists:flatten(io_lib:format("~p", [Cause]));
error_cause(Cause) ->
    lists:flatten(io_lib:format("~p", [Cause])).


stream_data_self(#httpdb{timeout = T} = HttpDb, Params, Worker, ReqId, Cb) ->
    case accumulate_messages(ReqId, [], T + 500) of
    {Data, ibrowse_async_response} ->
        ibrowse:stream_next(ReqId),
        {Data, fun() -> stream_data_self(HttpDb, Params, Worker, ReqId, Cb) end};
    {Data, ibrowse_async_response_end} ->
        put(?STREAM_STATUS, ended),
        {Data, fun() -> throw({maybe_retry_req, more_data_expected}) end}
    end.

accumulate_messages(ReqId, Acc, Timeout) ->
    receive
    {ibrowse_async_response, ReqId, {error, Error}} ->
        throw({maybe_retry_req, Error});
    {ibrowse_async_response, ReqId, <<>>} ->
        accumulate_messages(ReqId, Acc, Timeout);
    {ibrowse_async_response, ReqId, Data} ->
        accumulate_messages(ReqId, [Data | Acc], 0);
    {ibrowse_async_response_end, ReqId} ->
        {iolist_to_binary(lists:reverse(Acc)), ibrowse_async_response_end}
    after Timeout ->
        % Note: ibrowse should always reply with timeouts, but this doesn't
        % seem to be always true when there's a very high rate of requests
        % and many open connections.
        if Acc =:= [] ->
            throw({maybe_retry_req, timeout});
        true ->
            {iolist_to_binary(lists:reverse(Acc)), ibrowse_async_response}
        end
    end.


full_url(#httpdb{url = BaseUrl}, Params) ->
    Path = proplists:get_value(path, Params, []),
    QueryArgs = proplists:get_value(qs, Params, []),
    BaseUrl ++ Path ++ query_args_to_string(QueryArgs, []).


query_args_to_string([], []) ->
    "";
query_args_to_string([], Acc) ->
    "?" ++ string:join(lists:reverse(Acc), "&");
query_args_to_string([{K, V} | Rest], Acc) ->
    query_args_to_string(Rest, [K ++ "=" ++ couch_httpd:quote(V) | Acc]).


oauth_header(#httpdb{oauth = nil}, _ConnParams) ->
    [];
oauth_header(#httpdb{url = BaseUrl, oauth = OAuth}, ConnParams) ->
    Consumer = {
        OAuth#oauth.consumer_key,
        OAuth#oauth.consumer_secret,
        OAuth#oauth.signature_method
    },
    Method = case proplists:get_value(method, ConnParams, get) of
    get -> "GET";
    post -> "POST";
    put -> "PUT";
    head -> "HEAD"
    end,
    QSL = proplists:get_value(qs, ConnParams, []),
    OAuthParams = oauth:sign(Method,
        BaseUrl ++ proplists:get_value(path, ConnParams, []),
        QSL, Consumer, OAuth#oauth.token, OAuth#oauth.token_secret) -- QSL,
    [{"Authorization",
        "OAuth " ++ oauth:header_params_encode(OAuthParams)}].


do_redirect(_Worker, Code, Headers, #httpdb{url = Url} = HttpDb, Params, _Cb) ->
    RedirectUrl = redirect_url(Headers, Url),
    {HttpDb2, Params2} = after_redirect(RedirectUrl, Code, HttpDb, Params),
    throw({retry, HttpDb2, Params2}).


redirect_url(RespHeaders, OrigUrl) ->
    MochiHeaders = mochiweb_headers:make(RespHeaders),
    RedUrl = mochiweb_headers:get_value("Location", MochiHeaders),
    #url{
        host = Host,
        host_type = HostType,
        port = Port,
        path = Path,  % includes query string
        protocol = Proto
    } = ibrowse_lib:parse_url(RedUrl),
    #url{
        username = User,
        password = Passwd
    } = ibrowse_lib:parse_url(OrigUrl),
    Creds = case is_list(User) andalso is_list(Passwd) of
    true ->
        User ++ ":" ++ Passwd ++ "@";
    false ->
        []
    end,
    HostPart = case HostType of
    ipv6_address ->
        "[" ++ Host ++ "]";
    _ ->
        Host
    end,
    atom_to_list(Proto) ++ "://" ++ Creds ++ HostPart ++ ":" ++
        integer_to_list(Port) ++ Path.

after_redirect(RedirectUrl, 303, HttpDb, Params) ->
    after_redirect(RedirectUrl, HttpDb, ?replace(Params, method, get));
after_redirect(RedirectUrl, _Code, HttpDb, Params) ->
    after_redirect(RedirectUrl, HttpDb, Params).

after_redirect(RedirectUrl, HttpDb, Params) ->
    Params2 = lists:keydelete(path, 1, lists:keydelete(qs, 1, Params)),
    {HttpDb#httpdb{url = RedirectUrl}, Params2}.
