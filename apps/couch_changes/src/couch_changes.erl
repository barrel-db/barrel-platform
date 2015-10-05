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

-module(couch_changes).
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("couch_changes.hrl").

-export([handle_changes/3]).

% For the builtin filter _docs_ids, this is the maximum number
% of documents for which we trigger the optimized code path.
-define(MAX_DOC_IDS, 100).

-record(changes_acc, {
    db,
    seq,
    prepend,
    filter,
    callback,
    user_acc,
    resp_type,
    limit,
    include_docs,
    doc_options,
    conflicts,
    timeout,
    timeout_fun
}).

%% @type Req -> #httpd{} | {json_req, JsonObj()}
handle_changes(Args1, Req, Db0) ->
    #changes_args{
        style = Style,
        filter = FilterName,
        feed = Feed,
        dir = Dir,
        since = Since
    } = Args1,


    {Start, Args} = case FilterName of
        "_view" ->
            couch_log:info("filter is a view~n", []),
            ViewArgs = make_view_args(Req),
            {DDoc, VName} = parse_view_param(Req),
            Args2 = Args1#changes_args{filter_fun= no_filter_fun(Style),
                                       filter_args=ViewArgs,
                                       view_name={DDoc, VName}},
            Start1 = fun() ->
                             {ok, Infos} = couch_mrview:get_view_info(Db0#db.name,
                                                                      DDoc, VName),
                             StartSeq = case Dir of
                                            rev ->
                                                proplists:get_value(group_seq,
                                                                    Infos, 0);
                                            fwd ->
                                                Since
                                        end,
                             {ok, Db} = couch_db:reopen(Db0),
                             {Db, StartSeq}
                     end,
            {Start1, Args2};
        _ ->
            {FilterFun, FilterArgs} = make_filter_fun(FilterName, Style, Req, Db0),
            Args2 = Args1#changes_args{filter_fun = FilterFun,
                                      filter_args = FilterArgs},
            Start1 = fun() ->
                            {ok, Db} = couch_db:reopen(Db0),
                            StartSeq = case Dir of
                                           rev ->
                                               couch_db:get_update_seq(Db);
                                           fwd ->
                                               Since
                                       end,
                            {Db, StartSeq}
                    end,
            {Start1, Args2}
    end,

    % begin timer to deal with heartbeat when filter function fails
    case Args#changes_args.heartbeat of
    undefined ->
        erlang:erase(last_changes_heartbeat);
    Val when is_integer(Val); Val =:= true ->
        put(last_changes_heartbeat, now())
    end,

    case lists:member(Feed, ["continuous", "longpoll", "eventsource"]) of
    true ->
        fun(CallbackAcc) ->
            {Callback, UserAcc} = get_callback_acc(CallbackAcc),
            Self = self(),

            {Event, DDocId} = case FilterName of
                "_view" ->
                    #changes_args{view_name={DDocId1, _}} = Args,
                    %% start indexer
                    couch_index_server:acquire_indexer(couch_mrview_index,
                                                       Db0#db.name, DDocId1),
                    %% subscribe to event
                    couch_event:subscribe_cond(index_update,
                                               [{{'_', '$1', '$2', '_'},
                                                 [{'==', '$1', Db0#db.name},
                                                  {'==', '$2', DDocId1}],
                                                 [true]}]),

                    {index_update, DDocId1};
                _ ->
                    couch_event:subscribe_cond(db_updated, [{{'$1', '_'},
                                                             [{'==', '$1', Db0#db.name}],
                                                             [true]}]),
                    {db_updated, nil}
            end,
            {Db, StartSeq} = Start(),
            UserAcc2 = start_sending_changes(Callback, UserAcc, Feed),
            {Timeout, TimeoutFun} = get_changes_timeout(Args, Callback),
            Acc0 = build_acc(Args, Callback, UserAcc2, Db, StartSeq,
                             <<"">>, Timeout, TimeoutFun),
            try
                keep_sending_changes(
                    Args#changes_args{dir=fwd},
                    Acc0,
                    true)
            after
                couch_event:unsubscribe(Event),
                case FilterName of
                    "_view" ->
                        couch_index_server:release_indexer(couch_mrview_index,
                                                           Db0#db.name,
                                                           DDocId);
                    _ ->
                        ok
                end,
                get_rest_db_updated(ok) % clean out any remaining update messages
            end
        end;
    false ->
        fun(CallbackAcc) ->
            {Callback, UserAcc} = get_callback_acc(CallbackAcc),
            UserAcc2 = start_sending_changes(Callback, UserAcc, Feed),
            {Timeout, TimeoutFun} = get_changes_timeout(Args, Callback),
            {Db, StartSeq} = Start(),
            Acc0 = build_acc(Args#changes_args{feed="normal"}, Callback,
                             UserAcc2, Db, StartSeq, <<>>, Timeout, TimeoutFun),
            {ok, #changes_acc{seq = LastSeq, user_acc = UserAcc3}} =
                send_changes(
                    Args#changes_args{feed="normal"},
                    Acc0,
                    true),
            end_sending_changes(Callback, UserAcc3, LastSeq, Feed)
        end
    end.

make_view_args(#httpd{method=Method}=Req) ->
    Query = couch_httpd:qs(Req),
    parse_view_options(Query, false, []);
make_view_args({json_req, {Props}}=Req) ->
    {Query} = couch_util:get_value(<<"query">>, Props, {[]}),
    parse_view_options(Query, true, []).


get_callback_acc({Callback, _UserAcc} = Pair) when is_function(Callback, 3) ->
    Pair;
get_callback_acc(Callback) when is_function(Callback, 2) ->
    {fun(Ev, Data, _) -> Callback(Ev, Data) end, ok}.

%% @type Req -> #httpd{} | {json_req, JsonObj()}
make_filter_fun([$_ | _] = FilterName, Style, Req, Db) ->
    builtin_filter_fun(FilterName, Style, Req, Db);
make_filter_fun(FilterName, Style, Req, Db) ->
    {os_filter_fun(FilterName, Style, Req, Db), []}.


no_filter_fun(Style) ->
    fun(Db, #doc_info{revs=Revs}) ->
        builtin_results(Style, Revs)
    end.


os_filter_fun(FilterName, Style, Req, Db) ->
    case [list_to_binary(couch_httpd:unquote(Part))
            || Part <- string:tokens(FilterName, "/")] of
    [] ->
        fun(_Db2, #doc_info{revs=Revs}) ->
                builtin_results(Style, Revs)
        end;
    [DName, FName] ->
        DesignId = <<"_design/", DName/binary>>,
        DDoc = couch_httpd_db:couch_doc_open(Db, DesignId, nil, [ejson_body]),
        % validate that the ddoc has the filter fun
        #doc{body={Props}} = DDoc,
        couch_util:get_nested_json_value({Props}, [<<"filters">>, FName]),
        fun(Db2, DocInfo) ->
            DocInfos =
            case Style of
            main_only ->
                [DocInfo];
            all_docs ->
                [DocInfo#doc_info{revs=[Rev]}|| Rev <- DocInfo#doc_info.revs]
            end,
            Docs = [Doc || {ok, Doc} <- [
                    couch_db:open_doc(Db2, DocInfo2, [deleted, conflicts])
                        || DocInfo2 <- DocInfos]],
            {ok, Passes} = couch_query_servers:filter_docs(
                Req, Db2, DDoc, FName, Docs
            ),
            [{[{<<"rev">>, couch_doc:rev_to_str({RevPos,RevId})}]}
                || {Pass, #doc{revs={RevPos,[RevId|_]}}}
                <- lists:zip(Passes, Docs), Pass == true]
        end;
    _Else ->
        throw({bad_request,
            "filter parameter must be of the form `designname/filtername`"})
    end.

builtin_filter_fun("_doc_ids", Style, {json_req, {Props}}, _Db) ->
    DocIds = couch_util:get_value(<<"doc_ids">>, Props),
    {filter_docids(DocIds, Style), DocIds};
builtin_filter_fun("_doc_ids", Style, #httpd{method='POST'}=Req, _Db) ->
    {Props} = couch_httpd:json_body_obj(Req),
    DocIds =  couch_util:get_value(<<"doc_ids">>, Props, nil),
    {filter_docids(DocIds, Style), DocIds};
builtin_filter_fun("_doc_ids", Style, #httpd{method='GET'}=Req, _Db) ->
    DocIds = ?JSON_DECODE(couch_httpd:qs_value(Req, "doc_ids", "null")),
    {filter_docids(DocIds, Style), DocIds};
builtin_filter_fun("_design", Style, _Req, _Db) ->
    {filter_designdoc(Style), []};
builtin_filter_fun(_FilterName, _Style, _Req, _Db) ->
    throw({bad_request, "unknown builtin filter name"}).

filter_docids(DocIds, Style) when is_list(DocIds)->
    fun(_Db, #doc_info{id=DocId, revs=Revs}) ->
            case lists:member(DocId, DocIds) of
                true ->
                    builtin_results(Style, Revs);
                _ -> []
            end
    end;
filter_docids(_, _) ->
    throw({bad_request, "`doc_ids` filter parameter is not a list."}).

filter_designdoc(Style) ->
    fun(_Db, #doc_info{id=DocId, revs=Revs}) ->
            case DocId of
            <<"_design", _/binary>> ->
                    builtin_results(Style, Revs);
                _ -> []
            end
    end.

builtin_results(Style, [#rev_info{rev=Rev}|_]=Revs) ->
    case Style of
        main_only ->
            [{[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}];
        all_docs ->
            [{[{<<"rev">>, couch_doc:rev_to_str(R)}]}
                || #rev_info{rev=R} <- Revs]
    end.

get_changes_timeout(Args, Callback) ->
    #changes_args{
        heartbeat = Heartbeat,
        timeout = Timeout,
        feed = ResponseType
    } = Args,
    DefaultTimeout = list_to_integer(
        couch_config:get("httpd", "changes_timeout", "60000")
    ),
    case Heartbeat of
    undefined ->
        case Timeout of
        undefined ->
            {DefaultTimeout, fun(UserAcc) -> {stop, UserAcc} end};
        infinity ->
            {infinity, fun(UserAcc) -> {stop, UserAcc} end};
        _ ->
            {lists:min([DefaultTimeout, Timeout]),
                fun(UserAcc) -> {stop, UserAcc} end}
        end;
    true ->
        {DefaultTimeout,
            fun(UserAcc) -> {ok, Callback(timeout, ResponseType, UserAcc)} end};
    _ ->
        {lists:min([DefaultTimeout, Heartbeat]),
            fun(UserAcc) -> {ok, Callback(timeout, ResponseType, UserAcc)} end}
    end.

start_sending_changes(_Callback, UserAcc, ResponseType)
        when ResponseType =:= "continuous"
        orelse ResponseType =:= "eventsource" ->
    UserAcc;
start_sending_changes(Callback, UserAcc, ResponseType) ->
    Callback(start, ResponseType, UserAcc).

build_acc(Args, Callback, UserAcc, Db, StartSeq, Prepend, Timeout, TimeoutFun) ->
    #changes_args{
        include_docs = IncludeDocs,
        doc_options = DocOpts,
        conflicts = Conflicts,
        limit = Limit,
        feed = ResponseType,
        filter_fun = FilterFun
    } = Args,
    #changes_acc{
        db = Db,
        seq = StartSeq,
        prepend = Prepend,
        filter = FilterFun,
        callback = Callback,
        user_acc = UserAcc,
        resp_type = ResponseType,
        limit = Limit,
        include_docs = IncludeDocs,
        doc_options = DocOpts,
        conflicts = Conflicts,
        timeout = Timeout,
        timeout_fun = TimeoutFun
    }.

send_changes(Args, Acc0, FirstRound) ->
    #changes_args{
        dir = Dir,
        filter = FilterName,
        filter_args = FilterArgs,
        view_name = ViewName
    } = Args,
    #changes_acc{
        db = Db,
        seq = StartSeq
    } = Acc0,
    case FirstRound of
    true ->
        case FilterName of
        "_doc_ids" when length(FilterArgs) =< ?MAX_DOC_IDS ->
            send_changes_doc_ids(
                FilterArgs, Db, StartSeq, Dir, fun changes_enumerator/2, Acc0);
        "_design" ->
            send_changes_design_docs(
                Db, StartSeq, Dir, fun changes_enumerator/2, Acc0);
        "_view" ->
            {DDoc, VName} = ViewName,
            couch_mrview:view_changes_since(
              Db#db.name, DDoc, VName, StartSeq, fun view_changes_enumerator/2, FilterArgs, Acc0);

        _ ->
            couch_db:changes_since(
                Db, StartSeq, fun changes_enumerator/2, [{dir, Dir}], Acc0)
        end;
    false ->
        case FilterName of
            "_view" ->
                {DDoc, VName} = ViewName,
                io:format("filter args ~p~n", [FilterArgs]),
                couch_mrview:view_changes_since(
                    Db#db.name, DDoc, VName, StartSeq, fun view_changes_enumerator/2, FilterArgs, Acc0);
            _ ->
                couch_db:changes_since(
                    Db, StartSeq, fun changes_enumerator/2, [{dir, Dir}], Acc0)
        end
    end.


send_changes_doc_ids(DocIds, Db, StartSeq, Dir, Fun, Acc0) ->
    Lookups = couch_btree:lookup(Db#db.fulldocinfo_by_id_btree, DocIds),
    FullDocInfos = lists:foldl(
        fun({ok, FDI}, Acc) ->
            [FDI | Acc];
        (not_found, Acc) ->
            Acc
        end,
        [], Lookups),
    send_lookup_changes(FullDocInfos, StartSeq, Dir, Db, Fun, Acc0).


send_changes_design_docs(Db, StartSeq, Dir, Fun, Acc0) ->
    FoldFun = fun(FullDocInfo, _, Acc) ->
        {ok, [FullDocInfo | Acc]}
    end,
    KeyOpts = [{start_key, <<"_design/">>}, {end_key_gt, <<"_design0">>}],
    {ok, _, FullDocInfos} = couch_btree:fold(
        Db#db.fulldocinfo_by_id_btree, FoldFun, [], KeyOpts),
    send_lookup_changes(FullDocInfos, StartSeq, Dir, Db, Fun, Acc0).


send_lookup_changes(FullDocInfos, StartSeq, Dir, Db, Fun, Acc0) ->
    FoldFun = case Dir of
    fwd ->
        fun lists:foldl/3;
    rev ->
        fun lists:foldr/3
    end,
    GreaterFun = case Dir of
    fwd ->
        fun(A, B) -> A > B end;
    rev ->
        fun(A, B) -> A =< B end
    end,
    DocInfos = lists:foldl(
        fun(FDI, Acc) ->
            DI = couch_doc:to_doc_info(FDI),
            case GreaterFun(DI#doc_info.high_seq, StartSeq) of
            true ->
                [DI | Acc];
            false ->
                Acc
            end
        end,
        [], FullDocInfos),
    SortedDocInfos = lists:keysort(#doc_info.high_seq, DocInfos),
    FinalAcc = try
        FoldFun(
            fun(DocInfo, Acc) ->
                case Fun(DocInfo, Acc) of
                {ok, NewAcc} ->
                    NewAcc;
                {stop, NewAcc} ->
                    throw({stop, NewAcc})
                end
            end,
            Acc0, SortedDocInfos)
    catch
    throw:{stop, Acc} ->
        Acc
    end,
    case Dir of
    fwd ->
        {ok, FinalAcc#changes_acc{seq = couch_db:get_update_seq(Db)}};
    rev ->
        {ok, FinalAcc}
    end.


keep_sending_changes(Args, Acc0, FirstRound) ->
    #changes_args{
        feed = ResponseType,
        limit = Limit,
        db_open_options = DbOptions
    } = Args,

    {ok, ChangesAcc} = send_changes(Args#changes_args{dir=fwd}, Acc0, FirstRound),
    #changes_acc{
        db = Db, callback = Callback, timeout = Timeout, timeout_fun = TimeoutFun,
        seq = EndSeq, prepend = Prepend2, user_acc = UserAcc2, limit = NewLimit
    } = ChangesAcc,

    couch_db:close(Db),
    if Limit > NewLimit, ResponseType == "longpoll" ->
        end_sending_changes(Callback, UserAcc2, EndSeq, ResponseType);
    true ->
        case wait_db_updated(Timeout, TimeoutFun, UserAcc2) of
        {updated, UserAcc4} ->
            DbOptions1 = [{user_ctx, Db#db.user_ctx} | DbOptions],
            case couch_db:open(Db#db.name, DbOptions1) of
            {ok, Db2} ->
                keep_sending_changes(
                  Args#changes_args{limit=NewLimit},
                  ChangesAcc#changes_acc{
                    db = Db2,
                    user_acc = UserAcc4,
                    seq = EndSeq,
                    prepend = Prepend2,
                    timeout = Timeout,
                    timeout_fun = TimeoutFun},
                  false);
            _Else ->
                end_sending_changes(Callback, UserAcc2, EndSeq, ResponseType)
            end;
        {stop, UserAcc4} ->
            end_sending_changes(Callback, UserAcc4, EndSeq, ResponseType)
        end
    end.

end_sending_changes(Callback, UserAcc, EndSeq, ResponseType) ->
    Callback({stop, EndSeq}, ResponseType, UserAcc).

view_changes_enumerator({{Seq, _Key, DocId}, Val}, Acc) ->
    #changes_acc{db = Db0} = Acc,
    {ok, Db} = couch_db:reopen(Db0),


    case couch_db:get_doc_info(Db, DocId) of
        {ok, DocInfo} when Val /= removed ->
           changes_enumerator(DocInfo, Acc, Seq);
        {ok, _DocInfo} ->
            {ok, Acc};
        {error, not_found} ->
            {ok, Acc};
        not_found ->
            {ok, Acc};
        Error ->
            throw(Error)
    end.

changes_enumerator(DocInfo, Acc) ->
    changes_enumerator(DocInfo, Acc, nil).

changes_enumerator(DocInfo, #changes_acc{resp_type = ResponseType} = Acc,
                   ViewSeq)
        when ResponseType =:= "continuous"
        orelse ResponseType =:= "eventsource" ->
    #changes_acc{
        filter = FilterFun, callback = Callback,
        user_acc = UserAcc, limit = Limit, db = Db,
        timeout = Timeout, timeout_fun = TimeoutFun
    } = Acc,
    #doc_info{high_seq = Seq0} = DocInfo,
    Seq = use_seq(Seq0, ViewSeq),
    Results0 = FilterFun(Db, DocInfo),
    Results = [Result || Result <- Results0, Result /= null],
    %% TODO: I'm thinking this should be < 1 and not =< 1
    Go = if Limit =< 1 -> stop; true -> ok end,
    case Results of
    [] ->
        {Done, UserAcc2} = maybe_heartbeat(Timeout, TimeoutFun, UserAcc),
        case Done of
        stop ->
            {stop, Acc#changes_acc{seq = Seq, user_acc = UserAcc2}};
        ok ->
            {Go, Acc#changes_acc{seq = Seq, user_acc = UserAcc2}}
        end;
    _ ->
        ChangesRow = changes_row(Results, DocInfo, Acc, Seq),
        UserAcc2 = Callback({change, ChangesRow, <<>>}, ResponseType, UserAcc),
        reset_heartbeat(),
        {Go, Acc#changes_acc{seq = Seq, user_acc = UserAcc2, limit = Limit - 1}}
    end;
changes_enumerator(DocInfo, Acc, ViewSeq) ->
    #changes_acc{
        filter = FilterFun, callback = Callback, prepend = Prepend,
        user_acc = UserAcc, limit = Limit, resp_type = ResponseType, db = Db,
        timeout = Timeout, timeout_fun = TimeoutFun
    } = Acc,
    #doc_info{high_seq = Seq0} = DocInfo,
    Seq = use_seq(Seq0, ViewSeq),
    Results0 = FilterFun(Db, DocInfo),
    Results = [Result || Result <- Results0, Result /= null],
    Go = if (Limit =< 1) andalso Results =/= [] -> stop; true -> ok end,
    case Results of
    [] ->
        {Done, UserAcc2} = maybe_heartbeat(Timeout, TimeoutFun, UserAcc),
        case Done of
        stop ->
            {stop, Acc#changes_acc{seq = Seq, user_acc = UserAcc2}};
        ok ->
            {Go, Acc#changes_acc{seq = Seq, user_acc = UserAcc2}}
        end;
    _ ->
        ChangesRow = changes_row(Results, DocInfo, Acc, Seq),
        UserAcc2 = Callback({change, ChangesRow, Prepend}, ResponseType, UserAcc),
        reset_heartbeat(),
        {Go, Acc#changes_acc{
            seq = Seq, prepend = <<",\n">>,
            user_acc = UserAcc2, limit = Limit - 1}}
    end.


changes_row(Results, DocInfo, Acc, Seq) ->
    #doc_info{
        id = Id, revs = [#rev_info{deleted = Del} | _]
    } = DocInfo,
    #changes_acc{
        db = Db,
        include_docs = IncDoc,
        doc_options = DocOpts,
        conflicts = Conflicts
    } = Acc,
    {[{<<"seq">>, Seq}, {<<"id">>, Id}, {<<"changes">>, Results}] ++
        deleted_item(Del) ++ case IncDoc of
            true ->
                Opts = case Conflicts of
                    true -> [deleted, conflicts];
                    false -> [deleted]
                end,
                Doc = couch_index_util:load_doc(Db, DocInfo, Opts),
                case Doc of
                    null ->
                        [{doc, null}];
                    _ ->
                        [{doc, couch_doc:to_json_obj(Doc, DocOpts)}]
                end;
            false ->
                []
        end}.

deleted_item(true) -> [{<<"deleted">>, true}];
deleted_item(_) -> [].

use_seq(Seq, nil) -> Seq;
use_seq(_, ViewSeq) -> ViewSeq.

% waits for a db_updated msg, if there are multiple msgs, collects them.
wait_db_updated(Timeout, TimeoutFun, UserAcc) ->
    receive
        {couch_event, db_updated, _}=Ev ->
            couch_log:info("got db update ~p~n", [Ev]),
            get_rest_db_updated(UserAcc);
        {couch_event, index_update, _} ->
            get_rest_db_updated(UserAcc)
    after Timeout ->
              {Go, UserAcc2} = TimeoutFun(UserAcc),
              case Go of
                  ok ->
                      wait_db_updated(Timeout, TimeoutFun, UserAcc2);
                  stop ->
                      {stop, UserAcc2}
              end
    end.

get_rest_db_updated(UserAcc) ->
    receive
        {couch_event, db_updated, _} ->
            get_rest_db_updated(UserAcc);
        {couch_event, index_update, _} ->
            get_rest_db_updated(UserAcc)
    after 0 ->
              {updated, UserAcc}
    end.

reset_heartbeat() ->
    case get(last_changes_heartbeat) of
    undefined ->
        ok;
    _ ->
        put(last_changes_heartbeat, now())
    end.

maybe_heartbeat(Timeout, TimeoutFun, Acc) ->
    Before = get(last_changes_heartbeat),
    case Before of
    undefined ->
        {ok, Acc};
    _ ->
        Now = now(),
        case timer:now_diff(Now, Before) div 1000 >= Timeout of
        true ->
            Acc2 = TimeoutFun(Acc),
            put(last_changes_heartbeat, Now),
            Acc2;
        false ->
            {ok, Acc}
        end
    end.




%% view changes items
%%

parse_view_param({json_req, {Props}}) ->
    {Query} = couch_util:get_value(<<"query">>, Props),
    parse_view_param1(couch_util:get_value(<<"view">>, Query, <<"">>));
parse_view_param(Req) ->
    parse_view_param1(list_to_binary(couch_httpd:qs_value(Req, "view", ""))).

parse_view_param1(ViewParam) ->
    case re:split(ViewParam, <<"/">>) of
        [DName, ViewName] ->
            {<< "_design/", DName/binary >>, ViewName};
        _ ->
            throw({bad_request, "Invalid `view` parameter."})
    end.

parse_view_options([], _JsonReq, Acc) ->
    Acc;

parse_view_options(Options, true, Acc) ->
     %% get list of view attributes
    ViewFields0 = [couch_util:to_binary(F) || F <- record_info(fields,  mrargs)],
    ViewFields = [<<"key">> | ViewFields0],

    [{K, V} || {K, V} <- Options, lists:member(K, ViewFields)];
parse_view_options([{K, V} | Rest], JsonReq, Acc) ->
    Acc1 = case couch_util:to_binary(K) of
        <<"reduce">> ->
            [{reduce, couch_mrview_http:parse_boolean(V)}];
        <<"key">> ->
            V1 = parse_json(V, JsonReq),
            [{start_key, V1}, {end_key, V1} | Acc];
        <<"keys">> ->
            [{keys, parse_json(V, JsonReq)} | Acc];
        <<"startkey">> ->
            [{start_key, parse_json(V, JsonReq)} | Acc];
        <<"start_key">> ->
            [{start_key, parse_json(V, JsonReq)} | Acc];
        <<"startkey_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"start_key_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"endkey">> ->
            [{end_key, parse_json(V, JsonReq)} | Acc];
        <<"end_key">> ->
            [{end_key, parse_json(V, JsonReq)} | Acc];
        <<"endkey_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"end_key_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"limit">> ->
            [{limit, couch_mrview_http:parse_pos_int(V)} | Acc];
        <<"count">> ->
            throw({query_parse_error, <<"QS param `count` is not `limit`">>});
        <<"stale">> when V =:= <<"ok">> orelse V =:= "ok" ->
            [{stale, ok} | Acc];
        <<"stale">> when V =:= <<"update_after">> orelse V =:= "update_after" ->
            [{stale, update_after} | Acc];
        <<"stale">> ->
            throw({query_parse_error, <<"Invalid value for `stale`.">>});
        <<"descending">> ->
            case couch_mrview_http:parse_boolean(V) of
                true ->
                    [{direction, rev} | Acc];
                _ ->
                    [{direction, fwd} | Acc]
            end;
        <<"skip">> ->
            [{skip, couch_mrview_http:parse_pos_int(V)} | Acc];
        <<"group">> ->
            case couch_mrview_http:parse_booolean(V) of
                true ->
                    [{group_level, exact} | Acc];
                _ ->
                    [{group_level, 0} | Acc]
            end;
        <<"group_level">> ->
            [{group_level, couch_mrview_http:parse_pos_int(V)} | Acc];
        <<"inclusive_end">> ->
            [{inclusive_end, couch_mrview_http:parse_boolean(V)}];
        _ ->
            Acc
    end,
    parse_view_options(Rest, JsonReq, Acc1).

parse_json(V, true) when is_binary(V) ->
    ?JSON_DECODE(V);
parse_json(V, false) when is_list(V) ->
    ?JSON_DECODE(V);
parse_json(V, _) ->
    V.

