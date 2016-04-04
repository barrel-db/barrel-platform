-module(couch_httpd_request).

-include_lib("couch/include/couch_db.hrl").

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
    MaxSize = barrel_config:get_integer("couchdb", "max_document_size", 4294967296),
    MochiReq:recv_body(MaxSize);
body(#httpd{req_body=ReqBody}) ->
    ReqBody.
