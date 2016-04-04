-module(couch_httpd_all_docs).

-export([query_all_docs/2, query_all_docs/4]).
-export([handle_req/2]).
-export([parse_qs/2]).
-export([is_keys/1]).

-include_lib("couch/include/couch_db.hrl").

-include("couch_httpd.hrl").


-record(vacc, {db,
			   req,
			   resp,
			   prepend,
			   etag,
			   should_close=false}).


-record(fold_acc, {db,
        		   meta_sent=false,
        		   total_rows,
        		   offset,
        		   limit,
        		   skip,
        		   doc_info,
        		   callback,
        		   user_acc,
        		   last_go=ok,
        		   update_seq,
        		   args}).


query_all_docs(Db, Args) ->
    query_all_docs(Db, Args, fun default_cb/2, []).


query_all_docs(Db, Args, Callback, Acc) when is_list(Args) ->
    query_all_docs(Db, to_all_docs_args(Args), Callback, Acc);
query_all_docs(Db, Args0, Callback, Acc) ->
    Sig = couch_util:with_db(Db, fun(WDb) ->
        {ok, Info} = couch_db:get_db_info(WDb),
        couch_util:hexsig(couch_util:md5(term_to_binary(Info)))
    end),
    Args2 = validate_args(Args0),
    {ok, Acc1} = case Args2#all_docs_args.preflight_fun of
        PFFun when is_function(PFFun, 2) -> PFFun(Sig, Acc);
        _ -> {ok, Acc}
    end,
    all_docs_fold(Db, Args2, Callback, Acc1).

handle_req(#httpd{method='GET'}=Req, Db) ->
    all_docs_req(Req, Db, undefined);
handle_req(#httpd{method='POST'}=Req, Db) ->
    Keys = is_keys(couch_httpd:json_body_obj(Req)),
    all_docs_req(Req, Db, Keys);
handle_req(Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "GET,POST,HEAD").


all_docs_fold(Db, #all_docs_args{keys=undefined}=Args, Callback, UAcc) ->
    {ok, Info} = couch_db:get_db_info(Db),
    Total = couch_util:get_value(doc_count, Info),
    UpdateSeq = couch_db:get_update_seq(Db),
    Acc = #fold_acc{
        db=Db,
        total_rows=Total,
        limit=Args#all_docs_args.limit,
        skip=Args#all_docs_args.skip,
        callback=Callback,
        user_acc=UAcc,
        update_seq=UpdateSeq,
        args=Args
    },
    [Opts] = all_docs_key_opts(Args),
    {ok, Offset, FinalAcc} = couch_db:enum_docs(Db, fun all_docs_fold1/3, Acc, Opts),
    finish_fold(FinalAcc, [{total, Total}, {offset, Offset}]);
all_docs_fold(Db, #all_docs_args{direction=Dir, keys=Keys0}=Args, Callback, UAcc) ->
    {ok, Info} = couch_db:get_db_info(Db),
    Total = couch_util:get_value(doc_count, Info),
    UpdateSeq = couch_db:get_update_seq(Db),
    Acc = #fold_acc{
        db=Db,
        total_rows=Total,
        limit=Args#all_docs_args.limit,
        skip=Args#all_docs_args.skip,
        callback=Callback,
        user_acc=UAcc,
        update_seq=UpdateSeq,
        args=Args
    },
    % Backwards compatibility hack. The old _all_docs iterates keys
    % in reverse if descending=true was passed. Here we'll just
    % reverse the list instead.
    Keys = if Dir =:= fwd -> Keys0; true -> lists:reverse(Keys0) end,

    FoldFun = fun(Key, Acc0) ->
        DocInfo = (catch couch_db:get_doc_info(Db, Key)),
        {Doc, Acc1} = case DocInfo of
            {ok, #doc_info{id=Id, revs=[RevInfo | _RestRevs]}=DI} ->
                Rev = couch_doc:rev_to_str(RevInfo#rev_info.rev),
                Props = [{rev, Rev}] ++ case RevInfo#rev_info.deleted of
                    true -> [{deleted, true}];
                    false -> []
                end,
                {{{Id, Id}, {Props}}, Acc0#fold_acc{doc_info=DI}};
            not_found ->
                {{{Key, error}, not_found}, Acc0}
        end,
        {_, Acc2} = all_docs_fold1(Doc, {[], [{0, 0, 0}]}, Acc1),
        Acc2
    end,
    FinalAcc = lists:foldl(FoldFun, Acc, Keys),
    finish_fold(FinalAcc, [{total, Total}]).


all_docs_req(Req, Db, Keys) ->
    case couch_db:is_system_db(Db) of
    true ->
        case (catch couch_db:check_is_admin(Db)) of
        ok ->
            do_all_docs_req(Req, Db, Keys);
        _ ->
            DbName = ?b2l(Db#db.name),
            case barrel_config:get("couch_httpd_auth", "authentication_db", "_users") of
            DbName ->
                UsersDbPublic = barrel_config:get("couch_httpd_auth", "users_db_public", "false"),
                PublicFields = barrel_config:get("couch_httpd_auth", "public_fields"),
                case {UsersDbPublic, PublicFields} of
                {"true", PublicFields} when PublicFields =/= undefined ->
                    do_all_docs_req(Req, Db, Keys);
                {_, _} ->
                    throw({forbidden, <<"Only admins can access _all_docs",
                                        " of system databases.">>})
                end;
            _ ->
                throw({forbidden, <<"Only admins can access _all_docs",
                                    " of system databases.">>})
            end
        end;
    false ->
        do_all_docs_req(Req, Db, Keys)
    end.

do_all_docs_req(Req, Db, Keys) ->
    Args0 = parse_qs(Req, Keys),
    ETagFun = fun(Sig, Acc0) ->
        ETag = couch_httpd:make_etag(Sig),
        case couch_httpd:etag_match(Req, ETag) of
            true -> throw({etag_match, ETag});
            false -> {ok, Acc0#vacc{etag=ETag}}
        end
    end,
    Args = Args0#all_docs_args{preflight_fun=ETagFun},
    {ok, Resp} = couch_httpd:etag_maybe(Req, fun() ->
        VAcc0 = #vacc{db=Db, req=Req},
        DbName = ?b2l(Db#db.name),
        UsersDbName = barrel_config:get("couch_httpd_auth", "authentication_db", "_users"),
        IsAdmin = is_admin(Db),
        Callback = get_view_callback(DbName, UsersDbName, IsAdmin),
        query_all_docs(Db, Args, Callback, VAcc0)
    end),
    case is_record(Resp, vacc) of
        true -> {ok, Resp#vacc.resp};
        _ -> {ok, Resp}
    end.



all_docs_fold1(#full_doc_info{} = FullDocInfo, OffsetReds, Acc) ->
    % matches for _all_docs and translates #full_doc_info{} -> KV pair
    case couch_doc:to_doc_info(FullDocInfo) of
        #doc_info{id=Id, revs=[#rev_info{deleted=false, rev=Rev}|_]} = DI ->
            Value = {[{rev, couch_doc:rev_to_str(Rev)}]},
            all_docs_fold1({{Id, Id}, Value}, OffsetReds, Acc#fold_acc{doc_info=DI});
        #doc_info{revs=[#rev_info{deleted=true}|_]} ->
            {ok, Acc}
    end;
all_docs_fold1(_KV, _Offset, #fold_acc{skip=N}=Acc) when N > 0 ->
    {ok, Acc#fold_acc{skip=N-1, last_go=ok}};
all_docs_fold1(KV, OffsetReds, #fold_acc{offset=undefined}=Acc) ->
    #fold_acc{
        total_rows=Total,
        callback=Callback,
        user_acc=UAcc0,
        update_seq=UpdateSeq,
        args=Args
    } = Acc,
    Offset = all_docs_reduce_to_count(OffsetReds),
    Meta = make_meta(Args, UpdateSeq, [{total, Total}, {offset, Offset}]),
    {Go, UAcc1} = Callback(Meta, UAcc0),
    Acc1 = Acc#fold_acc{meta_sent=true, offset=Offset, user_acc=UAcc1, last_go=Go},
    case Go of
        ok -> all_docs_fold1(KV, OffsetReds, Acc1);
        stop -> {stop, Acc1}
    end;
all_docs_fold1(_KV, _Offset, #fold_acc{limit=0}=Acc) ->
    {stop, Acc};
all_docs_fold1({{_Key, _Id}, {removed, _Seq}}, _Offset, Acc) ->
    {ok, Acc#fold_acc{last_go=ok}};
all_docs_fold1({{Key, Id}, Val}, _Offset, Acc) ->
    #fold_acc{
        db=Db,
        limit=Limit,
        doc_info=DI,
        callback=Callback,
        user_acc=UAcc0,
        args=Args
    } = Acc,
    Doc = case DI of
        #doc_info{} -> maybe_load_doc(Db, DI, Args);
        _ -> maybe_load_doc(Db, Id, Val, Args)
    end,
    Row = [{id, Id}, {key, Key}, {value, Val}] ++ Doc,
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#fold_acc{
        limit=Limit-1,
        doc_info=undefined,
        user_acc=UAcc1,
        last_go=Go
    }}.


finish_fold(#fold_acc{last_go=ok, update_seq=UpdateSeq}=Acc,  ExtraMeta) ->
    #fold_acc{callback=Callback, user_acc=UAcc, args=Args}=Acc,
    % Possible send meta info
    Meta = make_meta(Args, UpdateSeq, ExtraMeta),
    {Go, UAcc1} = case Acc#fold_acc.meta_sent of
        false -> Callback(Meta, UAcc);
        _ -> {ok, Acc#fold_acc.user_acc}
    end,
    % Notify callback that the fold is complete.
    {_, UAcc2} = case Go of
        ok -> Callback(complete, UAcc1);
        _ -> {ok, UAcc1}
    end,
    {ok, UAcc2};
finish_fold(#fold_acc{user_acc=UAcc}, _ExtraMeta) ->
    {ok, UAcc}.

% admin users always get all fields
get_view_callback(_, _, true) ->
    fun view_cb/2;
% if we are operating on the users db and we aren't
% admin, filter the view
get_view_callback(_DbName, _DbName, false) ->
    fun filtered_view_cb/2;
% non _users databases get all fields
get_view_callback(_, _, _) ->
    fun view_cb/2.


filtered_view_cb({row, Row0}, Acc) ->
  Row1 = lists:map(fun({doc, null}) ->
        {doc, null};
    ({doc, Body}) ->
        Doc = couch_users_db:strip_non_public_fields(#doc{body=Body}),
        {doc, Doc#doc.body};
    (KV) ->
        KV
    end, Row0),
    view_cb({row, Row1}, Acc);
filtered_view_cb(Obj, Acc) ->
    view_cb(Obj, Acc).

default_cb(complete, Acc) ->
    {ok, lists:reverse(Acc)};
default_cb({final, Info}, []) ->
    {ok, [Info]};
default_cb({final, _}, Acc) ->
    {ok, Acc};
default_cb(Row, Acc) ->
    {ok, [Row | Acc]}.



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

maybe_load_doc(_Db, _DI, #all_docs_args{include_docs=false}) ->
    [];
maybe_load_doc(Db, #doc_info{}=DI, #all_docs_args{conflicts=true, doc_options=Opts}) ->
    doc_row(couch_doc:load(Db, DI, [conflicts]), Opts);
maybe_load_doc(Db, #doc_info{}=DI, #all_docs_args{doc_options=Opts}) ->
    doc_row(couch_doc:load(Db, DI, []), Opts).


maybe_load_doc(_Db, _Id, _Val, #all_docs_args{include_docs=false}) ->
    [];
maybe_load_doc(Db, Id, Val, #all_docs_args{conflicts=true, doc_options=Opts}) ->
    doc_row(couch_doc:load(Db, docid_rev(Id, Val), [conflicts]), Opts);
maybe_load_doc(Db, Id, Val, #all_docs_args{doc_options=Opts}) ->
    doc_row(couch_doc:load(Db, docid_rev(Id, Val), []), Opts).

doc_row(null, _Opts) ->
    [{doc, null}];
doc_row(Doc, Opts) ->
    [{doc, couch_doc:to_json_obj(Doc, Opts)}].

docid_rev(Id, {Props}) ->
    DocId = couch_util:get_value(<<"_id">>, Props, Id),
    Rev = case couch_util:get_value(<<"_rev">>, Props, nil) of
        nil -> nil;
        Rev0 -> couch_doc:parse_rev(Rev0)
    end,
    {DocId, Rev};
docid_rev(Id, _) ->
    {Id, nil}.

make_meta(Args, UpdateSeq, Base) ->
    case Args#all_docs_args.update_seq of
        true -> {meta, Base ++ [{update_seq, UpdateSeq}]};
        _ -> {meta, Base}
    end.


all_docs_reduce_to_count(Reductions) ->
    Reduce = fun couch_db_updater:btree_by_id_reduce/2,
    {Count, _, _} = couch_btree:final_reduce(Reduce, Reductions),
    Count.

all_docs_key_opts(Args) ->
    all_docs_key_opts(Args, []).


all_docs_key_opts(#all_docs_args{keys=undefined}=Args, Extra) ->
    all_docs_key_opts(Args#all_docs_args{keys=[]}, Extra);
all_docs_key_opts(#all_docs_args{keys=[], direction=Dir}=Args, Extra) ->
    [[{dir, Dir}] ++ ad_skey_opts(Args) ++ ad_ekey_opts(Args) ++ Extra];
all_docs_key_opts(#all_docs_args{keys=Keys, direction=Dir}=Args, Extra) ->
    lists:map(fun(K) ->
        [{dir, Dir}]
        ++ ad_skey_opts(Args#all_docs_args{start_key=K})
        ++ ad_ekey_opts(Args#all_docs_args{end_key=K})
        ++ Extra
    end, Keys).


ad_skey_opts(#all_docs_args{start_key=SKey}) when is_binary(SKey) ->
    [{start_key, SKey}];
ad_skey_opts(#all_docs_args{start_key_docid=SKeyDocId}) ->
    [{start_key, SKeyDocId}].


ad_ekey_opts(#all_docs_args{end_key=EKey}=Args) when is_binary(EKey) ->
    Type = if Args#all_docs_args.inclusive_end -> end_key; true -> end_key_gt end,
    [{Type, EKey}];
ad_ekey_opts(#all_docs_args{end_key_docid=EKeyDocId}=Args) ->
    Type = if Args#all_docs_args.inclusive_end -> end_key; true -> end_key_gt end,
    [{Type, EKeyDocId}].




%% parse arguments

is_keys({Props}) ->
    case couch_util:get_value(<<"keys">>, Props) of
        undefined ->
            barrel_log:debug("POST with no keys member.", []),
            undefined;
        Keys when is_list(Keys) ->
            Keys;
        _ ->
            throw({bad_request, "`keys` member must be a array."})
    end.

to_all_docs_args(KeyList) ->
    lists:foldl(fun
                    ({dir, Value}, Acc) ->
                        Index = lookup_index(couch_util:to_existing_atom(direction)),
                        setelement(Index, Acc, Value);
                    ({Key, Value}, Acc) ->
                        Index = lookup_index(couch_util:to_existing_atom(Key)),
                        setelement(Index, Acc, Value)
                end, #all_docs_args{}, KeyList).

lookup_index(Key) ->
    Index = lists:zip(
        record_info(fields, all_docs_args), lists:seq(2, record_info(size, all_docs_args))
    ),
    couch_util:get_value(Key, Index).

parse_qs(#httpd{}=Req, Keys) ->
    parse_qs(couch_httpd:qs(Req), Keys);
parse_qs(Props, Keys) ->
    Args = #all_docs_args{},
    parse_qs(Props, Keys, Args).

parse_qs(Props, Keys, #all_docs_args{}=Args0) ->
    parse_qs(Props, Keys, Args0, false).

parse_qs(Props, Keys, #all_docs_args{}=Args0, Json) ->
    Args = Args0#all_docs_args{keys=Keys},
    lists:foldl(fun({K, V}, Acc) ->
        parse_param(K, V, Acc, Json)
    end, Args, Props).

parse_param(Key, Val, Args, Json) when is_binary(Key) ->
    parse_param(binary_to_list(Key), Val, Args, Json);
parse_param(Key, Val, Args, Json) ->
    case Key of
        "" ->
            Args;
        "key" ->
            JsonKey = couch_httpd_util:parse_json(Val, Json),
            Args#all_docs_args{start_key=JsonKey, end_key=JsonKey};
        "keys" ->
            Args#all_docs_args{keys=couch_httpd_util:parse_json(Val, Json)};
        "startkey" ->
            Args#all_docs_args{start_key=couch_httpd_util:parse_json(Val, Json)};
        "start_key" ->
            Args#all_docs_args{start_key=couch_httpd_util:parse_json(Val, Json)};
        "startkey_docid" ->
            Args#all_docs_args{start_key_docid=list_to_binary(Val)};
        "start_key_doc_id" ->
            Args#all_docs_args{start_key_docid=list_to_binary(Val)};
        "endkey" ->
            Args#all_docs_args{end_key=couch_httpd_util:parse_json(Val, Json)};
        "end_key" ->
            Args#all_docs_args{end_key=couch_httpd_util:parse_json(Val, Json)};
        "endkey_docid" ->
            Args#all_docs_args{end_key_docid=list_to_binary(Val)};
        "end_key_doc_id" ->
            Args#all_docs_args{end_key_docid=list_to_binary(Val)};
        "limit" ->
            Args#all_docs_args{limit=couch_httpd_util:parse_pos_int(Val)};
        "count" ->
            throw({query_parse_error, <<"QS param `count` is not `limit`">>});
        "stale" when Val == "ok" ->
            Args#all_docs_args{stale=ok};
        "stale" when Val == "update_after" ->
            Args#all_docs_args{stale=update_after};
        "stale" ->
            throw({query_parse_error, <<"Invalid value for `stale`.">>});
        "descending" ->
            case couch_httpd_util:parse_boolean(Val) of
                true -> Args#all_docs_args{direction=rev};
                _ -> Args#all_docs_args{direction=fwd}
            end;
        "skip" ->
            Args#all_docs_args{skip=couch_httpd_util:parse_pos_int(Val)};
        "inclusive_end" ->
            Args#all_docs_args{inclusive_end=couch_httpd_util:parse_boolean(Val)};
        "include_docs" ->
            Args#all_docs_args{include_docs=couch_httpd_util:parse_boolean(Val)};
        "attachments" ->
            case couch_httpd_util:parse_boolean(Val) of
            true ->
                Opts = Args#all_docs_args.doc_options,
                Args#all_docs_args{doc_options=[attachments|Opts]};
            false ->
                Args
            end;
        "att_encoding_info" ->
            case couch_httpd_util:parse_boolean(Val) of
            true ->
                Opts = Args#all_docs_args.doc_options,
                Args#all_docs_args{doc_options=[att_encoding_info|Opts]};
            false ->
                Args
            end;
        "update_seq" ->
            Args#all_docs_args{update_seq=couch_httpd_util:parse_boolean(Val)};
        "conflicts" ->
            Args#all_docs_args{conflicts=couch_httpd_util:parse_boolean(Val)};
        "list" ->
            Args#all_docs_args{list=list_to_binary(Val)};
        "callback" ->
            Args#all_docs_args{callback=list_to_binary(Val)};
        _ ->
            BKey = list_to_binary(Key),
            BVal = list_to_binary(Val),
            Args#all_docs_args{extra=[{BKey, BVal} | Args#all_docs_args.extra]}
    end.

args_error(Mesg) ->
    throw({query_parse_error, Mesg}).

validate_args(Args) ->
    case Args#all_docs_args.keys of
        Keys when is_list(Keys) -> ok;
        undefined -> ok;
        _ -> args_error(<<"`keys` must be an array of strings.">>)
    end,

    case {Args#all_docs_args.keys, Args#all_docs_args.start_key} of
        {undefined, _} -> ok;
        {[], _} -> ok;
        {[_|_], undefined} -> ok;
        _ -> args_error(<<"`start_key` is incompatible with `keys`">>)
    end,

    case Args#all_docs_args.start_key_docid of
        undefined -> ok;
        SKDocId0 when is_binary(SKDocId0) -> ok;
        _ -> args_error(<<"`start_key_docid` must be a string.">>)
    end,

    case {Args#all_docs_args.keys, Args#all_docs_args.end_key} of
        {undefined, _} -> ok;
        {[], _} -> ok;
        {[_|_], undefined} -> ok;
        _ -> args_error(<<"`end_key` is incompatible with `keys`">>)
    end,

    case Args#all_docs_args.end_key_docid of
        undefined -> ok;
        EKDocId0 when is_binary(EKDocId0) -> ok;
        _ -> args_error(<<"`end_key_docid` must be a string.">>)
    end,

    case Args#all_docs_args.direction of
        fwd -> ok;
        rev -> ok;
        _ -> args_error(<<"Invalid direction.">>)
    end,

    case {Args#all_docs_args.limit >= 0, Args#all_docs_args.limit == undefined} of
        {true, _} -> ok;
        {_, true} -> ok;
        _ -> args_error(<<"`limit` must be a positive integer.">>)
    end,

    case Args#all_docs_args.skip < 0 of
        true -> args_error(<<"`skip` must be >= 0">>);
        _ -> ok
    end,

    case Args#all_docs_args.stale of
        ok -> ok;
        update_after -> ok;
        false -> ok;
        _ -> args_error(<<"Invalid value for `stale`.">>)
    end,

    case is_boolean(Args#all_docs_args.inclusive_end) of
        true -> ok;
        _ -> args_error(<<"Invalid value for `inclusive_end`.">>)
    end,
    case Args#all_docs_args.conflicts of
        undefined -> ok;
        V when is_boolean(V) -> ok;
        _ -> args_error(<<"Invalid value for `conflicts`.">>)
    end,

    SKDocId = case {Args#all_docs_args.direction, Args#all_docs_args.start_key_docid} of
        {fwd, undefined} -> <<>>;
        {rev, undefined} -> <<255>>;
        {_, SKDocId1} -> SKDocId1
    end,

    EKDocId = case {Args#all_docs_args.direction, Args#all_docs_args.end_key_docid} of
        {fwd, undefined} -> <<255>>;
        {rev, undefined} -> <<>>;
        {_, EKDocId1} -> EKDocId1
    end,

    Args#all_docs_args{
        start_key_docid=SKDocId,
        end_key_docid=EKDocId
    }.

is_admin(Db) ->
    case catch couch_db:check_is_admin(Db) of
    {unauthorized, _} ->
        false;
    ok ->
        true
    end.
