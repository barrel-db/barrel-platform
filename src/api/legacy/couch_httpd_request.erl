%% Copyright 2016, Benoit Chesneau
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

-module(couch_httpd_request).

-include_lib("couch_db.hrl").

%% interface to the httpd record
-export([method/1, mochi_req/1, peer/1, path_parts/1, requested_path_parts/1,
         req_body/1, user_ctx/1]).
-export([qs/1]).
-export([qs_value/2]).
-export([path/1]).
-export([raw_path/1]).
-export([headers/1]).
-export([header_value/2, header_value/3]).
-export([primary_header_value/2]).
-export([cookie/1]).

-export([get/2]).
-export([set/2]).

-export([recv/2]).
-export([recv_chunked/4]).

-export([body_length/1]).
-export([body/1]).

-export([json_req_obj/2, json_req_obj/3]).
-export([send_external_response/2]).


-type req() :: #httpd{}.
-export_type([req/0]).

method(#httpd{method=Method}) -> Method.
mochi_req(#httpd{mochi_req=MochiReq}) -> MochiReq.
peer(#httpd{peer=Peer}) -> Peer.
path_parts(#httpd{path_parts=Parts}) -> Parts.
requested_path_parts(#httpd{requested_path_parts=Parts}) -> Parts.
user_ctx(#httpd{user_ctx=UserCtx}) -> UserCtx.
req_body(#httpd{req_body=ReqBody}) -> ReqBody.

qs(#httpd{mochi_req=MochiReq}) ->
    MochiReq:parse_qs().

qs_value(Req, Key) ->
    qs_value(Req, Key, undefined).

qs_value(Req, Key, Default) ->
    proplists:get_value(Key, qs(Req), Default).

path(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(path).

raw_path(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(raw_path).

headers(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(headers).

header_value(#httpd{mochi_req=MochiReq}, Key) ->
    MochiReq:get_header_value(Key).

header_value(#httpd{mochi_req=MochiReq}, Key, Default) ->
    case MochiReq:get_header_value(Key) of
        undefined -> Default;
        Value -> Value
    end.

primary_header_value(#httpd{mochi_req=MochiReq}, Key) ->
    MochiReq:get_primary_header_value(Key).

cookie(#httpd{mochi_req=MochiReq}) ->
    MochiReq:parse_cookie().


-spec get(atom(), req()) -> any(); ([atom()], req()) -> any().
get(List, Req) when is_list(List) ->
	[g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
	g(Atom, Req).

g(method, #httpd{method=Ret}) -> Ret;
g(mochi_req, #httpd{mochi_req=Ret}) -> Ret;
g(peer, #httpd{peer=Ret}) -> Ret;
g(path_parts, #httpd{path_parts=Ret}) -> Ret;
g(requested_path_parts, #httpd{requested_path_parts=Ret}) -> Ret;
g(user_ctx, #httpd{user_ctx=Ret}) -> Ret;
g(req_body, #httpd{req_body=Ret}) -> Ret.

set([], Req) -> Req;
set([{method, Val}|Tail], Req) -> set(Tail, Req#httpd{method=Val});
set([{mochi_req, Val}|Tail], Req) -> set(Tail, Req#httpd{mochi_req=Val});
set([{peer, Val}|Tail], Req) -> set(Tail, Req#httpd{peer=Val});
set([{path_parts, Val}|Tail], Req) -> set(Tail, Req#httpd{path_parts=Val});
set([{requested_path_parts, Val}|Tail], Req) -> set(Tail, Req#httpd{requested_path_parts=Val});
set([{user_ctx, Val}|Tail], Req) -> set(Tail, Req#httpd{user_ctx=Val});
set([{req_body, Val}|Tail], Req) -> set(Tail, Req#httpd{req_body=Val}).

recv(#httpd{mochi_req=MochiReq}, Len) ->
MochiReq:recv(Len).

recv_chunked(#httpd{mochi_req=MochiReq}, MaxChunkSize, ChunkFun, InitState) ->
    % Fun is called once with each chunk
    % Fun({Length, Binary}, State)
    % called with Length == 0 on the last time.
    MochiReq:stream_body(MaxChunkSize, ChunkFun, InitState).

body_length(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(body_length).

body(#httpd{mochi_req=MochiReq, req_body=undefined}) ->
    MaxSize = barrel_server:get_env(max_document_size),
    MochiReq:recv_body(MaxSize);
body(#httpd{req_body=ReqBody}) ->
    ReqBody.

json_req_obj(Req, Db) -> json_req_obj(Req, Db, null).
json_req_obj(#httpd{mochi_req=Req,
    method=Method,
    requested_path_parts=RequestedPath,
    path_parts=Path,
    req_body=ReqBody
}, Db, DocId) ->
    Body = case ReqBody of
               undefined ->
                   MaxSize = barrel_server:get_env(max_document_size),
                   Req:recv_body(MaxSize);
               Else -> Else
           end,
    ParsedForm = case Req:get_primary_header_value("content-type") of
                     "application/x-www-form-urlencoded" ++ _ ->
                         case Body of
                             undefined -> [];
                             _ -> mochiweb_util:parse_qs(Body)
                         end;
                     _ ->
                         []
                 end,
    Headers = Req:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    {ok, Info} = couch_db:get_db_info(Db),

% add headers...
    #{<<"info">> => Info,
        <<"id">> => DocId,
        <<"uuid">> => barrel_uuids:new(),
        <<"method">> => Method,
        <<"requested_path">> => RequestedPath,
        <<"path">> => Path,
        <<"raw_path">> => list_to_binary(Req:get(raw_path)),
        <<"query">> => json_query_keys(to_json_terms(Req:parse_qs())),
        <<"headers">> => to_json_terms(Hlist),
        <<"body">> => Body,
        <<"peer">> => list_to_binary(Req:get(peer)),
        <<"form">> => to_json_terms(ParsedForm),
        <<"cookie">> => to_json_terms(Req:parse_cookie()),
        <<"userCtx">> => barrel_lib:json_user_ctx(Db),
        <<"secObj">> => couch_db:get_security(Db)}.

to_json_terms(Data) ->
    to_json_terms(Data, #{}).

to_json_terms([], O) -> O;
to_json_terms([{K, V} | R], O) ->
    to_json_terms(R, O#{ barrel_lib:to_binary(K) => list_to_binary(V)}).

json_query_keys(Json) ->
    maps:map(fun
                 (<<"startkey">>, V) -> ?JSON_DECODE(V);
                 (<<"endkey">>, V) -> ?JSON_DECODE(V);
                 (<<"key">>, V) -> ?JSON_DECODE(V);
                 (_, V) -> V
             end, Json).

send_external_response(Req, Response) ->
    #extern_resp_args{
        code = Code,
        data = Data,
        ctype = CType,
        headers = Headers,
        json = Json
    } = parse_external_response(Response),
    Headers1 = default_or_content_type(CType, Headers),
    case Json of
        nil ->
            couch_httpd:send_response(Req, Code, Headers1, Data);
        Json ->
            couch_httpd:send_json(Req, Code, Headers1, Json)
    end.

parse_external_response({Response}) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
            {"", _} ->
                Args;
            {<<"code">>, Value} ->
                Args#extern_resp_args{code=Value};
            {<<"stop">>, true} ->
                Args#extern_resp_args{stop=true};
            {<<"json">>, Value} ->
                Args#extern_resp_args{
                    json=Value,
                    ctype="application/json"};
            {<<"body">>, Value} ->
                Args#extern_resp_args{data=Value, ctype="text/html; charset=utf-8"};
            {<<"base64">>, Value} ->
                Args#extern_resp_args{
                    data=base64:decode(Value),
                    ctype="application/binary"
                };
            {<<"headers">>, {Headers}} ->
                NewHeaders = lists:map(fun({Header, HVal}) ->
                    {binary_to_list(Header), binary_to_list(HVal)}
                                       end, Headers),
                Args#extern_resp_args{headers=NewHeaders};
            _ -> % unknown key
                Msg = lists:flatten(io_lib:format("Invalid data from external server: ~p", [{Key, Value}])),
                throw({external_response_error, Msg})
        end
                end, #extern_resp_args{}, Response).

default_or_content_type(DefaultContentType, Headers) ->
    IsContentType = fun({X, _}) -> string:to_lower(X) == "content-type" end,
    case lists:any(IsContentType, Headers) of
        false ->
            [{"Content-Type", DefaultContentType} | Headers];
        true ->
            Headers
    end.
