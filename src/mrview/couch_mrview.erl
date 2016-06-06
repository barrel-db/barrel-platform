%% Copyright 2015-2016, Benoit Chesneau
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

-module(couch_mrview).

-export([query_view/3, query_view/4, query_view/6]).
-export([view_changes_since/6, view_changes_since/7]).
-export([count_view_changes_since/4, count_view_changes_since/5]).
-export([get_info/2]).
-export([get_view_info/3]).
-export([refresh/2]).
-export([trigger_update/2, trigger_update/3]).
-export([compact/2, compact/3, cancel_compaction/2]).
-export([cleanup/1]).

-include_lib("couch_db.hrl").
-include("couch_mrview.hrl").

-record(mracc, {
    db,
    meta_sent=false,
    total_rows,
    offset,
    limit,
    skip,
    group_level,
    doc_info,
    callback,
    user_acc,
    last_go=ok,
    reduce_fun,
    update_seq,
    args
}).

query_view(Db, DDoc, VName) ->
    query_view(Db, DDoc, VName, #mrargs{}).


query_view(Db, DDoc, VName, Args) when is_list(Args) ->
    query_view(Db, DDoc, VName, to_mrargs(Args), fun default_cb/2, []);
query_view(Db, DDoc, VName, Args) ->
    query_view(Db, DDoc, VName, Args, fun default_cb/2, []).


query_view(Db, DDoc, VName, Args, Callback, Acc) when is_list(Args) ->
    query_view(Db, DDoc, VName, to_mrargs(Args), Callback, Acc);
query_view(Db, DDoc, VName, Args0, Callback, Acc0) ->
    {ok, VInfo, Sig, Args} = couch_mrview_util:get_view(Db, DDoc, VName, Args0),
    {ok, Acc1} = case Args#mrargs.preflight_fun of
        PFFun when is_function(PFFun, 2) -> PFFun(Sig, Acc0);
        _ -> {ok, Acc0}
    end,
    query_view(Db, VInfo, Args, Callback, Acc1).


query_view(Db, {Type, View}, Args, Callback, Acc) ->
    case Type of
        map -> map_fold(Db, View, Args, Callback, Acc);
        red -> red_fold(Db, View, Args, Callback, Acc)
    end.

view_changes_since(Db, DDoc, VName, StartSeq, Fun, Acc) ->
    view_changes_since(Db, DDoc, VName, StartSeq, Fun, [], Acc).

view_changes_since(Db, DDoc, VName, StartSeq, UserFun, Options, Acc) ->
    IdxType = changes_idx(Options),
    Args0 = make_view_changes_args(Options, IdxType),
    {ok, {_, View}, _, Args} = couch_mrview_util:get_view(Db, DDoc, VName,Args0),
    OptList = make_view_changes_opts(StartSeq, Options, Args, IdxType),
    Btree = case IdxType of
                by_seq -> View#mrview.seq_btree;
                by_key -> View#mrview.kseq_btree
            end,
    FoldFun = changes_fold_fun(IdxType, UserFun, Args, StartSeq),

    AccOut = lists:foldl(fun(Opts, Acc1) ->
                {ok, _R, Acc2} = couch_mrview_util:fold_changes(
                                   Btree, FoldFun, Acc1, Opts, IdxType),
                Acc2
        end, Acc, OptList),
    {ok, AccOut}.

count_view_changes_since(Db, DDoc, VName, SinceSeq) ->
    count_view_changes_since(Db, DDoc, VName, SinceSeq, []).

count_view_changes_since(Db, DDoc, VName, SinceSeq, Options) ->
    IdxType = changes_idx(Options),
    case IdxType of
        by_seq ->
            Args0 = make_view_changes_args(Options, IdxType),
            {ok, {_, View}, _, Args} = couch_mrview_util:get_view(Db, DDoc, VName, Args0),
            OptList = make_view_changes_opts(SinceSeq, Options, Args, IdxType),
            lists:foldl(
              fun(Opts, Acc0) ->
                      Bt= View#mrview.seq_btree,
                      {ok, N} = couch_btree:fold_reduce(
                                  Bt, fun(_SeqStart, PartialReds, 0) ->
                                              {ok, couch_btree:final_reduce(
                                                     Bt, PartialReds)}
                                      end,
                                  0, Opts),
                      Acc0 + N
              end, 0, OptList);
        by_key ->
            CountFun = fun(_, Acc) -> {ok, Acc + 1} end,
            {ok, Count} = view_changes_since(Db, DDoc, VName, SinceSeq,
                                             CountFun, Options, 0),
            Count
    end.

get_info(Db, DDoc) ->
    {ok, Pid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    couch_index:get_info(Pid).


%% get informations on a view
get_view_info(#db{name=DbName}, DDoc, VName) ->
    get_view_info(DbName, DDoc, VName);
get_view_info(DbName, DDoc, VName) ->
    {ok, {_, View}, _, _Args} = couch_mrview_util:get_view(DbName, DDoc, VName, #mrargs{direction=rev}),
    %% get the total number of rows
    {ok, TotalRows} =  couch_mrview_util:get_row_count(View),

    %% get the total number of sequence logged in this view
    SeqBtree = View#mrview.seq_btree,
    {ok, TotalSeqs} = case SeqBtree of
        nil -> {ok, 0};
        _ ->
            couch_btree:full_reduce(SeqBtree)
    end,

    {ok, [{update_seq, View#mrview.update_seq},
          {purge_seq, View#mrview.purge_seq},
          {last_seq, View#mrview.group_seq},
          {group_seq, View#mrview.group_seq},
          {total_rows, TotalRows},
          {total_seqs, TotalSeqs}]}.



%% @doc refresh a view index
refresh(#db{name=DbName}, DDoc) ->
    refresh(DbName, DDoc);

refresh(Db, DDoc) ->
    UpdateSeq = couch_util:with_db(Db, fun(WDb) ->
                    couch_db:get_update_seq(WDb)
            end),

    case couch_index_server:get_index(couch_mrview_index, Db, DDoc) of
        {ok, Pid} ->
            case catch couch_index:get_state(Pid, UpdateSeq) of
                {ok, _} -> ok;
                Error -> {error, Error}
            end;
        Error ->
            {error, Error}
    end.

trigger_update(#db{name=DbName}, DDoc) ->
    trigger_update(DbName, DDoc);

trigger_update(Db, DDoc) ->
    UpdateSeq = couch_util:with_db(Db, fun(WDb) ->
                    couch_db:get_update_seq(WDb)
            end),

    trigger_update(Db, DDoc, UpdateSeq).

trigger_update(Db, DDoc, UpdateSeq) ->
    {ok, Pid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    couch_index:trigger_update(Pid, UpdateSeq).


compact(Db, DDoc) ->
    compact(Db, DDoc, []).


compact(Db, DDoc, Opts) ->
    {ok, Pid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    couch_index:compact(Pid, Opts).


cancel_compaction(Db, DDoc) ->
    {ok, IPid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    {ok, CPid} = couch_index:get_compactor_pid(IPid),
    ok = couch_index_compactor:cancel(CPid),

    % Cleanup the compaction file if it exists
    {ok, #mrst{sig=Sig, db_name=DbName}} = couch_index:get_state(IPid, 0),
    couch_mrview_util:delete_compaction_file(DbName, Sig),
    ok.


cleanup(Db) ->
    couch_mrview_cleanup:run(Db).


map_fold(Db, View, Args, Callback, UAcc) ->
    {ok, Total} = couch_mrview_util:get_row_count(View),
    Acc = #mracc{
        db=Db,
        total_rows=Total,
        limit=Args#mrargs.limit,
        skip=Args#mrargs.skip,
        callback=Callback,
        user_acc=UAcc,
        reduce_fun=fun couch_mrview_util:reduce_to_count/1,
        update_seq=View#mrview.update_seq,
        args=Args
    },
    OptList = couch_mrview_util:key_opts(Args),
    {Reds, Acc2} = lists:foldl(fun(Opts, {_, Acc0}) ->
        {ok, R, A} = couch_mrview_util:fold(View, fun map_fold/3, Acc0, Opts),
        {R, A}
    end, {nil, Acc}, OptList),
    Offset = couch_mrview_util:reduce_to_count(Reds),
    finish_fold(Acc2, [{total, Total}, {offset, Offset}]).


map_fold(#full_doc_info{} = FullDocInfo, OffsetReds, Acc) ->
    % matches for _all_docs and translates #full_doc_info{} -> KV pair
    case barrel_doc:to_doc_info(FullDocInfo) of
        #doc_info{id=Id, revs=[#rev_info{deleted=false, rev=Rev}|_]} = DI ->
            Value = #{rev => barrel_doc:rev_to_str(Rev)},
            map_fold({{Id, Id}, Value}, OffsetReds, Acc#mracc{doc_info=DI});
        #doc_info{revs=[#rev_info{deleted=true}|_]} ->
            {ok, Acc}
    end;
map_fold(_KV, _Offset, #mracc{skip=N}=Acc) when N > 0 ->
    {ok, Acc#mracc{skip=N-1, last_go=ok}};
map_fold(KV, OffsetReds, #mracc{offset=undefined}=Acc) ->
    #mracc{
        total_rows=Total,
        callback=Callback,
        user_acc=UAcc0,
        reduce_fun=Reduce,
        update_seq=UpdateSeq,
        args=Args
    } = Acc,
    Offset = Reduce(OffsetReds),
    Meta = make_meta(Args, UpdateSeq, [{total, Total}, {offset, Offset}]),
    {Go, UAcc1} = Callback(Meta, UAcc0),
    Acc1 = Acc#mracc{meta_sent=true, offset=Offset, user_acc=UAcc1, last_go=Go},
    case Go of
        ok -> map_fold(KV, OffsetReds, Acc1);
        stop -> {stop, Acc1}
    end;
map_fold(_KV, _Offset, #mracc{limit=0}=Acc) ->
    {stop, Acc};
map_fold({{_Key, _Id}, {removed, _Seq}}, _Offset, Acc) ->
    {ok, Acc#mracc{last_go=ok}};
map_fold({{Key, Id}, Val}, _Offset, Acc) ->
    #mracc{
        db=Db,
        limit=Limit,
        doc_info=DI,
        callback=Callback,
        user_acc=UAcc0,
        args=Args
    } = Acc,
    Doc = case DI of
        #doc_info{} -> couch_mrview_util:maybe_load_doc(Db, DI, Args);
        _ -> couch_mrview_util:maybe_load_doc(Db, Id, Val, Args)
    end,
    Row = [{id, Id}, {key, Key}, {value, Val}] ++ Doc,
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{
        limit=Limit-1,
        doc_info=undefined,
        user_acc=UAcc1,
        last_go=Go
    }}.


red_fold(Db, {_Nth, _Lang, View}=RedView, Args, Callback, UAcc) ->
    Acc = #mracc{
        db=Db,
        total_rows=null,
        limit=Args#mrargs.limit,
        skip=Args#mrargs.skip,
        group_level=Args#mrargs.group_level,
        callback=Callback,
        user_acc=UAcc,
        update_seq=View#mrview.update_seq,
        args=Args
    },
    GroupFun = group_rows_fun(Args#mrargs.group_level),
    OptList = couch_mrview_util:key_opts(Args, [{key_group_fun, GroupFun}]),
    Acc2 = lists:foldl(fun(Opts, Acc0) ->
        {ok, Acc1} =
            couch_mrview_util:fold_reduce(RedView, fun red_fold/3,  Acc0, Opts),
        Acc1
    end, Acc, OptList),
    finish_fold(Acc2, []).

red_fold(_Key, _Red, #mracc{skip=N}=Acc) when N > 0 ->
    {ok, Acc#mracc{skip=N-1, last_go=ok}};
red_fold(Key, Red, #mracc{meta_sent=false}=Acc) ->
    #mracc{
        args=Args,
        callback=Callback,
        user_acc=UAcc0,
        update_seq=UpdateSeq
    } = Acc,
    Meta = make_meta(Args, UpdateSeq, []),
    {Go, UAcc1} = Callback(Meta, UAcc0),
    Acc1 = Acc#mracc{user_acc=UAcc1, meta_sent=true, last_go=Go},
    case Go of
        ok -> red_fold(Key, Red, Acc1);
        _ -> {Go, Acc1}
    end;
red_fold(_Key, _Red, #mracc{limit=0} = Acc) ->
    {stop, Acc};
red_fold(_Key, Red, #mracc{group_level=0} = Acc) ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, null}, {value, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}};
red_fold(Key, Red, #mracc{group_level=exact} = Acc) ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, Key}, {value, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}};
red_fold(K, Red, #mracc{group_level=I} = Acc) when I > 0, is_list(K) ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, lists:sublist(K, I)}, {value, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}};
red_fold(K, Red, #mracc{group_level=I} = Acc) when I > 0 ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, K}, {value, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}}.


finish_fold(#mracc{last_go=ok, update_seq=UpdateSeq}=Acc,  ExtraMeta) ->
    #mracc{callback=Callback, user_acc=UAcc, args=Args}=Acc,
    % Possible send meta info
    Meta = make_meta(Args, UpdateSeq, ExtraMeta),
    {Go, UAcc1} = case Acc#mracc.meta_sent of
        false -> Callback(Meta, UAcc);
        _ -> {ok, Acc#mracc.user_acc}
    end,
    % Notify callback that the fold is complete.
    {_, UAcc2} = case Go of
        ok -> Callback(complete, UAcc1);
        _ -> {ok, UAcc1}
    end,
    {ok, UAcc2};
finish_fold(#mracc{user_acc=UAcc}, _ExtraMeta) ->
    {ok, UAcc}.


make_meta(Args, UpdateSeq, Base) ->
    case Args#mrargs.update_seq of
        true -> {meta, Base ++ [{update_seq, UpdateSeq}]};
        _ -> {meta, Base}
    end.


group_rows_fun(exact) ->
    fun({Key1,_}, {Key2,_}) -> Key1 == Key2 end;
group_rows_fun(0) ->
    fun(_A, _B) -> true end;
group_rows_fun(GroupLevel) when is_integer(GroupLevel) ->
    fun({[_|_] = Key1,_}, {[_|_] = Key2,_}) ->
        lists:sublist(Key1, GroupLevel) == lists:sublist(Key2, GroupLevel);
    ({Key1,_}, {Key2,_}) ->
        Key1 == Key2
    end.


default_cb(complete, Acc) ->
    {ok, lists:reverse(Acc)};
default_cb({final, Info}, []) ->
    {ok, [Info]};
default_cb({final, _}, Acc) ->
    {ok, Acc};
default_cb(Row, Acc) ->
    {ok, [Row | Acc]}.


to_mrargs(KeyList) ->
    lists:foldl(fun
                    ({dir, Value}, Acc) ->
                        Index = lookup_index(couch_util:to_existing_atom(direction)),
                        setelement(Index, Acc, Value);
                    ({Key, Value}, Acc) ->
                        Index = lookup_index(couch_util:to_existing_atom(Key)),
                        setelement(Index, Acc, Value)
                end, #mrargs{}, KeyList).


lookup_index(Key) ->
    Index = lists:zip(
        record_info(fields, mrargs), lists:seq(2, record_info(size, mrargs))
    ),
    couch_util:get_value(Key, Index).


changes_idx(Options) ->
    ByKey = lists:any(fun({K, _}) ->
                lists:member(K, [start_key, end_key, start_key_docid,
                                 end_key_docid, keys, queries])
        end, Options),
    case ByKey of
        true -> by_key;
        false -> by_seq
    end.

make_view_changes_args(Options, by_key) ->
    to_mrargs(Options);
make_view_changes_args(_Option, _) ->
    #mrargs{}.

make_view_changes_opts(StartSeq, _Options, Args, by_key) ->
    couch_mrview_util:changes_key_opts(StartSeq, Args);
make_view_changes_opts(StartSeq, Options, #mrargs{direction=rev}, _) ->
    [[{start_key, StartSeq}] ++ Options];
make_view_changes_opts(StartSeq, Options, _Args, _) ->
    [[{start_key, StartSeq+1}] ++ Options].

changes_fold_fun(by_seq, UserFun, _Args, _StartSeq) ->
    UserFun;
changes_fold_fun(by_key, UserFun, Args, StartSeq) ->
    GreaterFun = case Args#mrargs.direction of
                     fwd -> fun(S) -> S > StartSeq end;
                     _ -> fun(S) -> S < StartSeq end
                 end,
    fun({{Seq, _Key, _DocId}, _Val}=KV, Acc) ->
            case GreaterFun(Seq) of
                true -> UserFun(KV, Acc);
                false -> {ok, Acc}
            end
    end.