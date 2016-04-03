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

-module(couch_mrview_http).

-export([
    handle_reindex_req/3,
    handle_view_req/3,
    handle_temp_view_req/2,
    handle_info_req/3,
    handle_compact_req/3,
    handle_cleanup_req/2,
    parse_qs/2
]).

-include_lib("couch/include/couch_db.hrl").

-include("couch_mrview.hrl").


-record(vacc, {
    db,
    req,
    resp,
    prepend,
    etag,
    should_close=false
}).


handle_reindex_req(#httpd{method='POST',
                          path_parts=[_, _, DName,<<"_reindex">>]}=Req,
                   Db, _DDoc) ->
    ok = couch_db:check_is_admin(Db),
    couch_mrview:trigger_update(Db, <<"_design/", DName/binary>>),
    couch_httpd:send_json(Req, 201, {[{<<"ok">>, true}]});
handle_reindex_req(Req, _Db, _DDoc) ->
    couch_httpd:send_method_not_allowed(Req, "POST").


handle_view_req(#httpd{method='GET',
                       path_parts=[_, _, _, _, VName, <<"_last_seq">>]}=Req,
                Db, DDoc) ->
    {ok, Info} = couch_mrview:get_view_info(Db, DDoc, VName),
    LastSeq = proplists:get_value(last_seq, Info, 0),
    couch_httpd:send_json(Req, 200, {[
        {name, VName},
        {last_seq, LastSeq}
    ]});
handle_view_req(#httpd{method='GET',
                      path_parts=[_, _, DDocName, _, VName, <<"_info">>]}=Req,
                Db, _DDoc) ->

    DDocId = <<"_design/", DDocName/binary >>,
    {ok, Info} = couch_mrview:get_view_info(Db#db.name, DDocId, VName),

    FinalInfo = [{db_name, Db#db.name},
                 {ddoc, DDocId},
                 {view, VName}] ++ Info,
    couch_httpd:send_json(Req, 200, {FinalInfo});
handle_view_req(#httpd{method='GET'}=Req, Db, DDoc) ->
    [_, _, _, _, ViewName] = Req#httpd.path_parts,
    exometer:update([httpd, view_reads], 1),
    design_doc_view(Req, Db, DDoc, ViewName, undefined);
handle_view_req(#httpd{method='POST'}=Req, Db, DDoc) ->
    [_, _, _, _, ViewName] = Req#httpd.path_parts,
    Props = couch_httpd:json_body_obj(Req),
    Keys = couch_httpd_all_docs:is_keys(Props),
    Queries = get_view_queries(Props),
    case {Queries, Keys} of
        {Queries, undefined} ->
            [exometer:update([httpd, view_reads], 1)
             || _I <- Queries],
            multi_query_view(Req, Db, DDoc, ViewName, Queries);
        {undefined, Keys} ->
            exometer:update([httpd, view_reads], 1),
            design_doc_view(Req, Db, DDoc, ViewName, Keys);
        {undefined, undefined} ->
            throw({
                bad_request,
                "POST body must contain `keys` or `queries` field"
            });
        {_, _} ->
            throw({bad_request, "`keys` and `queries` are mutually exclusive"})
    end;
handle_view_req(Req, _Db, _DDoc) ->
    couch_httpd:send_method_not_allowed(Req, "GET,POST,HEAD").


handle_temp_view_req(#httpd{method='POST'}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = couch_db:check_is_admin(Db),
    {Body} = couch_httpd:json_body_obj(Req),
    DDoc = couch_mrview_util:temp_view_to_ddoc({Body}),
    Keys = couch_httpd_all_docs:is_keys({Body}),
    exometer:update([httpd, temporary_view_reads], 1),
    design_doc_view(Req, Db, DDoc, <<"temp">>, Keys);
handle_temp_view_req(Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "POST").


handle_info_req(#httpd{method='GET'}=Req, Db, DDoc) ->
    [_, _, Name, _] = Req#httpd.path_parts,
    {ok, Info} = couch_mrview:get_info(Db, DDoc),
    couch_httpd:send_json(Req, 200, {[
        {name, Name},
        {view_index, {Info}}
    ]});
handle_info_req(Req, _Db, _DDoc) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


handle_compact_req(#httpd{method='POST'}=Req, Db, DDoc) ->
    ok = couch_db:check_is_admin(Db),
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = couch_mrview:compact(Db, DDoc),
    couch_httpd:send_json(Req, 202, {[{ok, true}]});
handle_compact_req(Req, _Db, _DDoc) ->
    couch_httpd:send_method_not_allowed(Req, "POST").


handle_cleanup_req(#httpd{method='POST'}=Req, Db) ->
    ok = couch_db:check_is_admin(Db),
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = couch_mrview:cleanup(Db),
    couch_httpd:send_json(Req, 202, {[{ok, true}]});
handle_cleanup_req(Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "POST").

design_doc_view(Req, Db, DDoc, ViewName, Keys) ->
    Args0 = parse_qs(Req, Keys),
    ETagFun = fun(Sig, Acc0) ->
        ETag = couch_httpd:make_etag(Sig),
        case couch_httpd:etag_match(Req, ETag) of
            true -> throw({etag_match, ETag});
            false -> {ok, Acc0#vacc{etag=ETag}}
        end
    end,
    Args = Args0#mrargs{preflight_fun=ETagFun},
    {ok, Resp} = couch_httpd:etag_maybe(Req, fun() ->
        VAcc0 = #vacc{db=Db, req=Req},
        couch_mrview:query_view(Db, DDoc, ViewName, Args, fun view_cb/2, VAcc0)
    end),
    case is_record(Resp, vacc) of
        true -> {ok, Resp#vacc.resp};
        _ -> {ok, Resp}
    end.

multi_query_view(Req, Db, DDoc, ViewName, Queries) ->
    Args0 = parse_qs(Req, undefined),
    {ok, _, _, Args1} = couch_mrview_util:get_view(Db, DDoc, ViewName, Args0),
    ArgQueries = lists:map(fun({Query}) ->
        QueryArg = parse_qs(Query, undefined, Args1, true),
        couch_mrview_util:validate_args(QueryArg)
    end, Queries),
    {ok, Resp2} = couch_httpd:etag_maybe(Req, fun() ->
                    VAcc0 = #vacc{db=Db, req=Req, prepend="\r\n"},
                    Etag = barrel_uuids:new(),
                    Headers = [{"ETag", Etag}],
                    FirstChunk = "{\"results\":[",
                    {ok, Resp} = couch_httpd:start_json_response(Req, 200, Headers),
                    couch_httpd:send_chunk(Resp, FirstChunk),
                    VAcc1 = VAcc0#vacc{resp=Resp},
                    VAcc2 = lists:foldl(fun(Args, Acc0) ->
                                    {ok, Acc1} = couch_mrview:query_view(
                                            Db, DDoc, ViewName, Args,
                                            fun view_cb/2,  Acc0),
                                    Acc1
                            end, VAcc1, ArgQueries),
                    couch_httpd:send_chunk(VAcc2#vacc.resp, "\r\n]}"),
                    {ok, Resp2} = couch_httpd:end_json_response(VAcc2#vacc.resp),
                    {ok, VAcc2#vacc{resp=Resp2}}
            end),

    case is_record(Resp2, vacc) of
        true -> {ok, Resp2#vacc.resp};
        _ -> {ok, Resp2}
    end.

view_cb({meta, Meta}, #vacc{resp=undefined}=Acc) ->
    Headers = [{"ETag", Acc#vacc.etag}],
    {ok, Resp} = couch_httpd:start_json_response(Acc#vacc.req, 200, Headers),
     view_cb({meta, Meta}, Acc#vacc{resp=Resp, should_close=true});
view_cb({meta, Meta}, #vacc{resp=Resp}=Acc) ->
    % Map function starting
    Parts = case couch_util:get_value(total, Meta) of
        undefined -> [];
        Total -> [io_lib:format("\"total_rows\":~p", [Total])]
    end ++ case couch_util:get_value(offset, Meta) of
        undefined -> [];
        Offset -> [io_lib:format("\"offset\":~p", [Offset])]
    end ++ case couch_util:get_value(update_seq, Meta) of
        undefined -> [];
        UpdateSeq -> [io_lib:format("\"update_seq\":~p", [UpdateSeq])]
    end ++ ["\"rows\":["],
    Chunk = lists:flatten("{" ++ string:join(Parts, ",") ++ "\r\n"),
    couch_httpd:send_chunk(Resp, Chunk),
    {ok, Acc#vacc{resp=Resp, prepend=""}};
view_cb({row, Row}, #vacc{resp=undefined}=Acc) ->
    case is_removed(Row) of
        true -> {ok, Acc};
        false ->
            % Reduce function starting
            Headers = [{"ETag", Acc#vacc.etag}],
            {ok, Resp} = couch_httpd:start_json_response(Acc#vacc.req, 200, Headers),
            couch_httpd:send_chunk(Resp, ["{\"rows\":[\r\n", row_to_json(Row)]),
            {ok, #vacc{resp=Resp, prepend=",\r\n"}}
    end;
view_cb({row, Row}, Acc) ->
    case is_removed(Row) of
        true -> {ok, Acc};
        false ->
            % Adding another row
            couch_httpd:send_chunk(Acc#vacc.resp, [Acc#vacc.prepend, row_to_json(Row)]),
            {ok, Acc#vacc{prepend=",\r\n"}}
    end;
view_cb(complete, #vacc{resp=undefined}=Acc) ->
    % Nothing in view
    {ok, Resp} = couch_httpd:send_json(Acc#vacc.req, 200, {[{rows, []}]}),
    {ok, Acc#vacc{resp=Resp}};


view_cb(complete, #vacc{resp=Resp}=Acc) ->
    % Finish view output
    couch_httpd:send_chunk(Resp, "\r\n]}"),
    case Acc#vacc.should_close of
        true ->
            {ok, Resp2} = couch_httpd:end_json_response(Resp),
            {ok, Acc#vacc{resp=Resp2}};
        _ ->
            {ok, Acc#vacc{resp=Resp, prepend=",\r\n"}}
    end.

is_removed(Row) ->
    proplists:get_value(value, Row) =:= removed.

row_to_json(Row) ->
    Id = couch_util:get_value(id, Row),
    row_to_json(Id, Row).


row_to_json(error, Row) ->
    % Special case for _all_docs request with KEYS to
    % match prior behavior.
    Key = couch_util:get_value(key, Row),
    Val = couch_util:get_value(value, Row),
    Obj = {[{key, Key}, {error, Val}]},
    ?JSON_ENCODE(Obj);
row_to_json(Id0, Row) ->
    Id = case Id0 of
        undefined -> [];
        Id0 -> [{id, Id0}]
    end,
    Key = couch_util:get_value(key, Row, null),
    Val = couch_util:get_value(value, Row),
    Doc = case couch_util:get_value(doc, Row) of
        undefined -> [];
        Doc0 -> [{doc, Doc0}]
    end,
    Obj = {Id ++ [{key, Key}, {value, Val}] ++ Doc},
    ?JSON_ENCODE(Obj).

get_view_queries({Props}) ->
    case couch_util:get_value(<<"queries">>, Props) of
        undefined ->
            undefined;
        Queries when is_list(Queries) ->
            Queries;
        _ ->
            throw({bad_request, "`queries` member must be a array."})
    end.


parse_qs(#httpd{}=Req, Keys) ->
    parse_qs(couch_httpd:qs(Req), Keys);
parse_qs(Props, Keys) ->
    Args = #mrargs{},
    parse_qs(Props, Keys, Args).

parse_qs(Props, Keys, #mrargs{}=Args0) ->
    parse_qs(Props, Keys, Args0, false).

parse_qs(Props, Keys, #mrargs{}=Args0, Json) ->
    Args = Args0#mrargs{keys=Keys},
    lists:foldl(fun({K, V}, Acc) ->
        parse_param(K, V, Acc, Json)
    end, Args, Props).

parse_param(Key, Val, Args, Json) when is_binary(Key) ->
    parse_param(binary_to_list(Key), Val, Args, Json);
parse_param(Key, Val, Args, Json) ->
    case Key of
        "" ->
            Args;
        "reduce" ->
            Args#mrargs{reduce=couch_httpd_util:parse_boolean(Val)};
        "key" ->
            JsonKey = couch_httpd_util:parse_json(Val, Json),
            Args#mrargs{start_key=JsonKey, end_key=JsonKey};
        "keys" ->
            Args#mrargs{keys=couch_httpd_util:parse_json(Val, Json)};
        "startkey" ->
            Args#mrargs{start_key=couch_httpd_util:parse_json(Val, Json)};
        "start_key" ->
            Args#mrargs{start_key=couch_httpd_util:parse_json(Val, Json)};
        "startkey_docid" ->
            Args#mrargs{start_key_docid=list_to_binary(Val)};
        "start_key_doc_id" ->
            Args#mrargs{start_key_docid=list_to_binary(Val)};
        "endkey" ->
            Args#mrargs{end_key=couch_httpd_util:parse_json(Val, Json)};
        "end_key" ->
            Args#mrargs{end_key=couch_httpd_util:parse_json(Val, Json)};
        "endkey_docid" ->
            Args#mrargs{end_key_docid=list_to_binary(Val)};
        "end_key_doc_id" ->
            Args#mrargs{end_key_docid=list_to_binary(Val)};
        "limit" ->
            Args#mrargs{limit=couch_httpd_util:parse_pos_int(Val)};
        "count" ->
            throw({query_parse_error, <<"QS param `count` is not `limit`">>});
        "stale" when Val == "ok" ->
            Args#mrargs{stale=ok};
        "stale" when Val == "update_after" ->
            Args#mrargs{stale=update_after};
        "stale" ->
            throw({query_parse_error, <<"Invalid value for `stale`.">>});
        "descending" ->
            case couch_httpd_util:parse_boolean(Val) of
                true -> Args#mrargs{direction=rev};
                _ -> Args#mrargs{direction=fwd}
            end;
        "skip" ->
            Args#mrargs{skip=couch_httpd_util:parse_pos_int(Val)};
        "group" ->
            case couch_httpd_util:parse_boolean(Val) of
                true -> Args#mrargs{group_level=exact};
                _ -> Args#mrargs{group_level=0}
            end;
        "group_level" ->
            Args#mrargs{group_level=couch_httpd_util:parse_pos_int(Val)};
        "inclusive_end" ->
            Args#mrargs{inclusive_end=couch_httpd_util:parse_boolean(Val)};
        "include_docs" ->
            Args#mrargs{include_docs=couch_httpd_util:parse_boolean(Val)};
        "attachments" ->
            case couch_httpd_util:parse_boolean(Val) of
            true ->
                Opts = Args#mrargs.doc_options,
                Args#mrargs{doc_options=[attachments|Opts]};
            false ->
                Args
            end;
        "att_encoding_info" ->
            case couch_httpd_util:parse_boolean(Val) of
            true ->
                Opts = Args#mrargs.doc_options,
                Args#mrargs{doc_options=[att_encoding_info|Opts]};
            false ->
                Args
            end;
        "update_seq" ->
            Args#mrargs{update_seq=couch_httpd_util:parse_boolean(Val)};
        "conflicts" ->
            Args#mrargs{conflicts=couch_httpd_util:parse_boolean(Val)};
        "list" ->
            Args#mrargs{list=list_to_binary(Val)};
        "callback" ->
            Args#mrargs{callback=list_to_binary(Val)};
        _ ->
            BKey = list_to_binary(Key),
            BVal = list_to_binary(Val),
            Args#mrargs{extra=[{BKey, BVal} | Args#mrargs.extra]}
    end.
