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

-module(couch_mrview_util).

-export([get_view/4]).
-export([ddoc_to_mrst/2, init_state/4, reset_index/3]).
-export([make_header/1]).
-export([index_file/2, compaction_file/2, open_file/1]).
-export([delete_files/2, delete_index_file/2, delete_compaction_file/2]).
-export([get_row_count/1, all_docs_reduce_to_count/1, reduce_to_count/1]).
-export([get_view_changes_count/1]).
-export([key_opts/1, key_opts/2]).
-export([fold/4, fold_reduce/4]).
-export([temp_view_to_ddoc/1]).
-export([calculate_data_size/2]).
-export([validate_args/1]).
-export([maybe_load_doc/3, maybe_load_doc/4]).
-export([changes_key_opts/2]).
-export([fold_changes/5]).
-export([to_key_seq/1]).

-define(MOD, couch_mrview_index).

-include_lib("couch_db.hrl").
-include("couch_mrview.hrl").


get_view(Db, DDoc, ViewName, Args0) ->
    ArgCheck = fun(InitState) ->
        Args1 = set_view_type(Args0, ViewName, InitState#mrst.views),
        {ok, validate_args(Args1)}
    end,
    {ok, Pid, Args2} = couch_index_server:get_index(?MOD, Db, DDoc, ArgCheck),
    DbUpdateSeq = barrel_lib:with_db(Db, fun(WDb) ->
        couch_db:get_update_seq(WDb)
    end),
    MinSeq = case Args2#mrargs.stale of
        ok -> 0; update_after -> 0; _ -> DbUpdateSeq
    end,
    {ok, State} = case couch_index:get_state(Pid, MinSeq) of
        {ok, _} = Resp -> Resp;
        Error -> throw(Error)
    end,
    couch_ref_counter:add(State#mrst.refc),
    if Args2#mrargs.stale == update_after ->
        spawn(fun() -> catch couch_index:get_state(Pid, DbUpdateSeq) end);
        true -> ok
    end,
    #mrst{language=Lang, views=Views} = State,
    {Type, View, Args3} = extract_view(Lang, Args2, ViewName, Views),
    check_range(Args3, view_cmp(View)),
    Sig = view_sig(Db, State, View, Args3),
    {ok, {Type, View}, Sig, Args3}.


ddoc_to_mrst(DbName, #doc{id=Id, body=Fields}) ->
    DesignOpts = maps:get(<<"options">>, Fields, #{}),
    RawViews = maps:get(<<"views">>, Fields, #{}),
    BySrc = maps:fold(fun(Name, MRFuns, BySrcAcc) ->
        case MRFuns of
            #{ <<"map">> := MapSrc } ->
                RedSrc = maps:get(<<"reduce">>, MRFuns, null),
                ViewOpts = maps:get(<<"options">>, MRFuns, #{}),
                View = case dict:find({MapSrc, ViewOpts}, BySrcAcc) of
                    {ok, View0} -> View0;
                    error -> #mrview{def=MapSrc, options=ViewOpts}
                end,
                {MapNames, RedSrcs} = case RedSrc of
                    null ->
                        MNames = [Name | View#mrview.map_names],
                        {MNames, View#mrview.reduce_funs};
                    _ ->
                        RedFuns = [{Name, RedSrc} | View#mrview.reduce_funs],
                        {View#mrview.map_names, RedFuns}
                    end,
                View2 = View#mrview{map_names=MapNames, reduce_funs=RedSrcs},
                dict:store({MapSrc, ViewOpts}, View2, BySrcAcc);
            _ ->
                BySrcAcc
        end
    end, dict:new(), RawViews),

    NumViews = fun({_, View}, N) ->
            {View#mrview{id_num=N}, N+1}
    end,
    {Views, _} = lists:mapfoldl(NumViews, 0, lists:sort(dict:to_list(BySrc))),

    Language = maps:get(<<"language">>, Fields, <<"javascript">>),
    Lib = maps:get(<<"lib">>, RawViews, #{}),

    IdxState = #mrst{
        db_name=DbName,
        idx_name=Id,
        lib=Lib,
        views=Views,
        language=Language,
        design_opts=DesignOpts
    },
    SigInfo = {?IVERSION, Views, Language, DesignOpts, couch_index_util:sort_lib(Lib)},
    {ok, IdxState#mrst{sig=crypto:hash(md5, term_to_binary(SigInfo))}}.


set_view_type(_Args, _ViewName, []) ->
    throw({not_found, missing_named_view});
set_view_type(Args, ViewName, [View | Rest]) ->
    RedNames = [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(ViewName, RedNames) of
        true ->
            case Args#mrargs.reduce of
                false -> Args#mrargs{view_type=map};
                _ -> Args#mrargs{view_type=red}
            end;
        false ->
            case lists:member(ViewName, View#mrview.map_names) of
                true -> Args#mrargs{view_type=map};
                false -> set_view_type(Args, ViewName, Rest)
            end
    end.


extract_view(_Lang, _Args, _ViewName, []) ->
    throw({not_found, missing_named_view});
extract_view(Lang, #mrargs{view_type=map}=Args, Name, [View | Rest]) ->
    Names = View#mrview.map_names ++ [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(Name, Names) of
        true -> {map, View, Args};
        _ -> extract_view(Lang, Args, Name, Rest)
    end;
extract_view(Lang, #mrargs{view_type=red}=Args, Name, [View | Rest]) ->
    RedNames = [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(Name, RedNames) of
        true -> {red, {index_of(Name, RedNames), Lang, View}, Args};
        false -> extract_view(Lang, Args, Name, Rest)
    end.


view_sig(Db, State, View, #mrargs{include_docs=true}=Args) ->
    BaseSig = view_sig(Db, State, View, Args#mrargs{include_docs=false}),
    UpdateSeq = couch_db:get_update_seq(Db),
    PurgeSeq = couch_db:get_purge_seq(Db),
    Term = view_sig_term(BaseSig, UpdateSeq, PurgeSeq),
    barrel_lib:hexsig(crypto:hash(md5, term_to_binary(Term)));
view_sig(Db, State, {_Nth, _Lang, View}, Args) ->
    view_sig(Db, State, View, Args);
view_sig(_Db, State, View, Args0) ->
    Sig = State#mrst.sig,
    UpdateSeq = View#mrview.update_seq,
    PurgeSeq = View#mrview.purge_seq,
    Args = Args0#mrargs{
        preflight_fun=undefined,
        extra=[]
    },
    Term = view_sig_term(Sig, UpdateSeq, PurgeSeq, Args),
    barrel_lib:hexsig(crypto:hash(md5, term_to_binary(Term))).

view_sig_term(BaseSig, UpdateSeq, PurgeSeq) ->
    {BaseSig, UpdateSeq, PurgeSeq}.

view_sig_term(BaseSig, UpdateSeq, PurgeSeq, Args) ->
    {BaseSig, UpdateSeq, PurgeSeq, Args}.


init_state(Db, Fd, #mrst{views=Views}=State, nil) ->
    Header = #mrheader{
        seq=0,
        group_seq=0,
        purge_seq=couch_db:get_purge_seq(Db),
        id_btree_state=nil,
        view_states=[{nil, nil, nil, 0, 0, 0} || _ <- Views]
    },
    init_state(Db, Fd, State, Header);
init_state(Db, Fd, State, Header) ->
    #mrst{
        language=Lang,
        views=Views
    } = State,

    #mrheader{
        seq=Seq,
        group_seq=GSeq,
        purge_seq=PurgeSeq,
        id_btree_state=IdBtreeState,
        view_states=ViewStates
    } = Header,

    StateUpdate = fun
        ({_, _,  _, _, _, _}=St) -> St;
        (St) -> {St, nil, nil, 0, 0, 0}
    end,
    ViewStates2 = lists:map(StateUpdate, ViewStates),

    IdBtOpts = [{compression, couch_db:compression(Db)}],
    {ok, IdBtree} = couch_btree:open(IdBtreeState, Fd, IdBtOpts),

    OpenViewFun = fun(St, View) -> open_view(Db, Fd, Lang, St, View) end,
    Views2 = lists:zipwith(OpenViewFun, ViewStates2, Views),

    State#mrst{
        fd=Fd,
        update_seq=Seq,
        group_seq=GSeq,
        purge_seq=PurgeSeq,
        id_btree=IdBtree,
        views=Views2
    }.

open_view(Db, Fd, Lang, {BTState, SeqBTState, KSeqBTState, USeq, PSeq, GSeq},
          View) ->
    FunSrcs = [FunSrc || {_Name, FunSrc} <- View#mrview.reduce_funs],
    ReduceFun =
        fun(reduce, KVs) ->
            KVs2 = detuple_kvs(expand_dups(KVs, []), []),
            KVs3 = [[K, V] || [K, V] <- KVs2, V /= removed],
            {ok, Result} = couch_query_servers:reduce(Lang, FunSrcs, KVs3),
            {length(KVs3), Result};
        (rereduce, Reds) ->
            Count = lists:sum([Count0 || {Count0, _} <- Reds]),
            UsrReds = [UsrRedsList || {_, UsrRedsList} <- Reds],
            {ok, Result} = couch_query_servers:rereduce(Lang, FunSrcs, UsrReds),
            {Count, Result}
        end,

    Less = case maps:get(<<"collation">>, View#mrview.options, undefind) of
        <<"raw">> -> fun(A, B) -> A < B end;
        _ -> fun couch_ejson_compare:less_json_ids/2
    end,

    ViewBtOpts = [
        {less, Less},
        {reduce, ReduceFun},
        {compression, couch_db:compression(Db)}
    ],
    {ok, Btree} = couch_btree:open(BTState, Fd, ViewBtOpts),

    BySeqReduceFun = fun couch_db_updater:btree_by_seq_reduce/2,
    ViewSeqBtOpts = [{reduce, BySeqReduceFun},
                     {compression, couch_db:compression(Db)}],
    {ok, SeqBtree} = couch_btree:open(SeqBTState, Fd, ViewSeqBtOpts),

    KSeqReduceFun = fun
                        (reduce, KVs) ->
                            length(KVs);
                        (rereduce, Reds) ->
                            lists:sum(Reds)
                    end,

    ViewKSeqBtOpts = [{less, Less},
                      {reduce, KSeqReduceFun},
                      {compression, couch_db:compression(Db)}],

    {ok, KSeqBtree} = couch_btree:open(KSeqBTState, Fd, ViewKSeqBtOpts),

    View#mrview{btree=Btree,
                seq_btree=SeqBtree,
                kseq_btree=KSeqBtree,
                update_seq=USeq,
                purge_seq=PSeq,
                group_seq=GSeq}.


temp_view_to_ddoc(Props) ->
    Language = maps:get(<<"language">>, Props, <<"javascript">>),
    Options = maps:get(<<"options">>, Props, #{}),
    View0 = #{<<"map">> => maps:get(<<"map">>, Props)},
    View1 = case Props of
        #{ <<"reduce">> := RedSrc} when is_binary(RedSrc) ->
            View0#{<<"reduce">> => RedSrc};
        _ ->
            View0
    end,
    DDoc = #{<<"_id">> => barrel_uuids:random(),
             <<"language">> => Language,
             <<"options">> => Options,
             <<"views">> => #{<<"temp">> => View1}},
    barrel_doc:from_json_obj(DDoc).


get_row_count(#mrview{btree=Bt}) ->
    {ok, {Count, _Reds}} = couch_btree:full_reduce(Bt),
    {ok, Count}.

all_docs_reduce_to_count(Reductions) ->
    Reduce = fun couch_db_updater:btree_by_id_reduce/2,
    {Count, _, _} = couch_btree:final_reduce(Reduce, Reductions),
    Count.

reduce_to_count(nil) ->
    0;
reduce_to_count(Reductions) ->
    Reduce = fun
        (reduce, KVs) ->
            Counts = [
                case V of {dups, Vals} -> length(Vals); _ -> 1 end
                || {_,V} <- KVs
            ],
            {lists:sum(Counts), []};
        (rereduce, Reds) ->
            {lists:sum([Count0 || {Count0, _} <- Reds]), []}
    end,
    {Count, _} = couch_btree:final_reduce(Reduce, Reductions),
    Count.

%% @doc get all changes for a view
get_view_changes_count(View) ->
    #mrview{seq_btree=SBtree} = View,
    CountFun = fun(_SeqStart, PartialReds, 0) ->
        {ok, couch_btree:final_reduce(SBtree, PartialReds)}
    end,
    case SBtree of
        #btree{} ->
            couch_btree:fold_reduce(SBtree, CountFun, 0, []);
        nil ->
            {ok, 0}
    end.

fold(#mrview{btree=Bt}, Fun, Acc, Opts) ->
    WrapperFun = fun(KV, Reds, Acc2) ->
        fold_fun(Fun, expand_dups([KV], []), Reds, Acc2)
    end,
    {ok, _LastRed, _Acc} = couch_btree:fold(Bt, WrapperFun, Acc, Opts).

fold_fun(_Fun, [], _, Acc) ->
    {ok, Acc};
fold_fun(Fun, [KV|Rest], {KVReds, Reds}, Acc) ->
    case Fun(KV, {KVReds, Reds}, Acc) of
        {ok, Acc2} ->
            fold_fun(Fun, Rest, {[KV|KVReds], Reds}, Acc2);
        {stop, Acc2} ->
            {stop, Acc2}
    end.


fold_changes(Bt, Fun, Acc, Opts, Type) ->
    WrapperFun = fun(KV, _Reds, Acc2) ->
        fold_changes_fun(Fun, changes_expand([KV], Type, []), Acc2)
    end,
    {ok, _LastRed, _Acc} = couch_btree:fold(Bt, WrapperFun, Acc, Opts).

fold_changes_fun(_Fun, [], Acc) ->
    {ok, Acc};
fold_changes_fun(Fun, [KV|Rest],  Acc) ->
    case Fun(KV, Acc) of
        {ok, Acc2} ->
            fold_changes_fun(Fun, Rest, Acc2);
        {stop, Acc2} ->
            {stop, Acc2}
    end.


fold_reduce({NthRed, Lang, View}, Fun,  Acc, Options) ->
    #mrview{
        btree=Bt,
        reduce_funs=RedFuns
    } = View,
    LPad = lists:duplicate(NthRed - 1, []),
    RPad = lists:duplicate(length(RedFuns) - NthRed, []),
    {_Name, FunSrc} = lists:nth(NthRed,RedFuns),

    ReduceFun = fun
        (reduce, KVs0) ->
            KVs1 = detuple_kvs(expand_dups(KVs0, []), []),
            KVs2 = [[K, V] || [K, V] <- KVs1, V /= removed],
            {ok, Red} = couch_query_servers:reduce(Lang, [FunSrc], KVs2),
            {0, LPad ++ Red ++ RPad};
        (rereduce, Reds) ->
            ExtractRed = fun({_, UReds0}) -> [lists:nth(NthRed, UReds0)] end,
            UReds = lists:map(ExtractRed, Reds),
            {ok, Red} = couch_query_servers:rereduce(Lang, [FunSrc], UReds),
            {0, LPad ++ Red ++ RPad}
    end,

    WrapperFun = fun({GroupedKey, _}, PartialReds, Acc0) ->
        {_, Reds} = couch_btree:final_reduce(ReduceFun, PartialReds),
        Fun(GroupedKey, lists:nth(NthRed, Reds), Acc0)
    end,

    couch_btree:fold_reduce(Bt, WrapperFun, Acc, Options).


validate_args(Args) ->
    Reduce = Args#mrargs.reduce,
    case Reduce == undefined orelse is_boolean(Reduce) of
        true -> ok;
        _ -> mrverror(<<"Invalid `reduce` value.">>)
    end,

    case {Args#mrargs.view_type, Reduce} of
        {map, true} -> mrverror(<<"Reduce is invalid for map-only views.">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, Args#mrargs.group_level, Args#mrargs.keys} of
        {red, exact, _} -> ok;
        {red, _, KeyList} when is_list(KeyList) ->
            Msg = <<"Multi-key fetchs for reduce views must use `group=true`">>,
            mrverror(Msg);
        _ -> ok
    end,

    case Args#mrargs.keys of
        Keys when is_list(Keys) -> ok;
        undefined -> ok;
        _ -> mrverror(<<"`keys` must be an array of strings.">>)
    end,

    case {Args#mrargs.keys, Args#mrargs.start_key} of
        {undefined, _} -> ok;
        {[], _} -> ok;
        {[_|_], undefined} -> ok;
        _ -> mrverror(<<"`start_key` is incompatible with `keys`">>)
    end,

    case Args#mrargs.start_key_docid of
        undefined -> ok;
        SKDocId0 when is_binary(SKDocId0) -> ok;
        _ -> mrverror(<<"`start_key_docid` must be a string.">>)
    end,

    case {Args#mrargs.keys, Args#mrargs.end_key} of
        {undefined, _} -> ok;
        {[], _} -> ok;
        {[_|_], undefined} -> ok;
        _ -> mrverror(<<"`end_key` is incompatible with `keys`">>)
    end,

    case Args#mrargs.end_key_docid of
        undefined -> ok;
        EKDocId0 when is_binary(EKDocId0) -> ok;
        _ -> mrverror(<<"`end_key_docid` must be a string.">>)
    end,

    case Args#mrargs.direction of
        fwd -> ok;
        rev -> ok;
        _ -> mrverror(<<"Invalid direction.">>)
    end,

    case {Args#mrargs.limit >= 0, Args#mrargs.limit == undefined} of
        {true, _} -> ok;
        {_, true} -> ok;
        _ -> mrverror(<<"`limit` must be a positive integer.">>)
    end,

    case Args#mrargs.skip < 0 of
        true -> mrverror(<<"`skip` must be >= 0">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, Args#mrargs.group_level} of
        {red, exact} -> ok;
        {_, 0} -> ok;
        {red, Int} when is_integer(Int), Int >= 0 -> ok;
        {red, _} -> mrverror(<<"`group_level` must be >= 0">>);
        {map, _} -> mrverror(<<"Invalid use of grouping on a map view.">>)
    end,

    case Args#mrargs.stale of
        ok -> ok;
        update_after -> ok;
        false -> ok;
        _ -> mrverror(<<"Invalid value for `stale`.">>)
    end,

    case is_boolean(Args#mrargs.inclusive_end) of
        true -> ok;
        _ -> mrverror(<<"Invalid value for `inclusive_end`.">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.include_docs} of
        {red, true} -> mrverror(<<"`include_docs` is invalid for reduce">>);
        {_, ID} when is_boolean(ID) -> ok;
        _ -> mrverror(<<"Invalid value for `include_docs`">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.conflicts} of
        {_, undefined} -> ok;
        {map, V} when is_boolean(V) -> ok;
        {red, undefined} -> ok;
        {map, _} -> mrverror(<<"Invalid value for `conflicts`.">>);
        {red, _} -> mrverror(<<"`conflicts` is invalid for reduce views.">>)
    end,

    SKDocId = case {Args#mrargs.direction, Args#mrargs.start_key_docid} of
        {fwd, undefined} -> <<>>;
        {rev, undefined} -> <<255>>;
        {_, SKDocId1} -> SKDocId1
    end,

    EKDocId = case {Args#mrargs.direction, Args#mrargs.end_key_docid} of
        {fwd, undefined} -> <<255>>;
        {rev, undefined} -> <<>>;
        {_, EKDocId1} -> EKDocId1
    end,

    Args#mrargs{
        start_key_docid=SKDocId,
        end_key_docid=EKDocId
    }.


check_range(#mrargs{start_key=undefined}, _Cmp) ->
    ok;
check_range(#mrargs{end_key=undefined}, _Cmp) ->
    ok;
check_range(#mrargs{start_key=K, end_key=K}, _Cmp) ->
    ok;
check_range(Args, Cmp) ->
    #mrargs{
        direction=Dir,
        start_key=SK,
        start_key_docid=SKD,
        end_key=EK,
        end_key_docid=EKD
    } = Args,
    case {Dir, Cmp({SK, SKD}, {EK, EKD})} of
        {fwd, false} ->
            throw({query_parse_error,
                <<"No rows can match your key range, reverse your ",
                    "start_key and end_key or set descending=true">>});
        {rev, true} ->
            throw({query_parse_error,
                <<"No rows can match your key range, reverse your ",
                    "start_key and end_key or set descending=false">>});
        _ -> ok
    end.


view_cmp({_Nth, _Lang, View}) ->
    view_cmp(View);
view_cmp(View) ->
    fun(A, B) -> couch_btree:less(View#mrview.btree, A, B) end.


make_header(State) ->
    #mrst{
        update_seq=Seq,
        purge_seq=PurgeSeq,
        group_seq=GroupSeq,
        id_btree=IdBtree,
        views=Views
    } = State,

    ViewStates = lists:foldr(fun(V, Acc) ->
                                     [{couch_btree:get_state(V#mrview.btree),
                                       couch_btree:get_state(V#mrview.seq_btree),
                                       couch_btree:get_state(V#mrview.kseq_btree),
                                       V#mrview.update_seq,
                                       V#mrview.purge_seq,
                                       V#mrview.group_seq} | Acc]
                             end, [], Views),

    #mrheader{
        seq=Seq,
        purge_seq=PurgeSeq,
        group_seq=GroupSeq,
        id_btree_state=couch_btree:get_state(IdBtree),
        view_states=ViewStates
    }.


index_file(DbName, Sig) ->
    FileName = barrel_lib:hexsig(Sig) ++ ".view",
    couch_index_util:index_file(mrview, DbName, FileName).


compaction_file(DbName, Sig) ->
    FileName = barrel_lib:hexsig(Sig) ++ ".compact.view",
    couch_index_util:index_file(mrview, DbName, FileName).


open_file(FName) ->
    case couch_file:open(FName, [nologifmissing]) of
        {ok, Fd} -> {ok, Fd};
        {error, enoent} -> couch_file:open(FName, [create]);
        Error -> Error
    end.


delete_files(DbName, Sig) ->
    delete_index_file(DbName, Sig),
    delete_compaction_file(DbName, Sig).


delete_index_file(DbName, Sig) ->
    delete_file(index_file(DbName, Sig)).


delete_compaction_file(DbName, Sig) ->
    delete_file(compaction_file(DbName, Sig)).


delete_file(FName) ->
    case filelib:is_file(FName) of
        true ->
            RootDir = couch_index_util:root_dir(),
            couch_file:delete(RootDir, FName);
        _ ->
            ok
    end.


reset_index(Db, Fd, #mrst{sig=Sig}=State) ->
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, {Sig, nil}),
    init_state(Db, Fd, reset_state(State), nil).


reset_state(State) ->
    State#mrst{
      fd=nil,
      qserver=nil,
      update_seq=0,
      id_btree=nil,
      views=[View#mrview{btree=nil,
                         seq_btree=nil,
                         kseq_btree=nil}
             || View <- State#mrst.views]
     }.


key_opts(Args) ->
    key_opts(Args, []).

key_opts(#mrargs{keys=undefined, direction=Dir}=Args, Extra) ->
    [[{dir, Dir}] ++ skey_opts(Args) ++ ekey_opts(Args) ++ Extra];
key_opts(#mrargs{keys=Keys, direction=Dir}=Args, Extra) ->
    lists:map(fun(K) ->
        [{dir, Dir}]
        ++ skey_opts(Args#mrargs{start_key=K})
        ++ ekey_opts(Args#mrargs{end_key=K})
        ++ Extra
    end, Keys).


skey_opts(#mrargs{start_key=undefined}) ->
    [];
skey_opts(#mrargs{start_key=SKey, start_key_docid=SKeyDocId}) ->
    [{start_key, {SKey, SKeyDocId}}].


ekey_opts(#mrargs{end_key=undefined}) ->
    [];
ekey_opts(#mrargs{end_key=EKey, end_key_docid=EKeyDocId}=Args) ->
    case Args#mrargs.inclusive_end of
        true -> [{end_key, {EKey, EKeyDocId}}];
        false -> [{end_key_gt, {EKey, reverse_key_default(EKeyDocId)}}]
    end.


reverse_key_default(<<>>) -> <<255>>;
reverse_key_default(<<255>>) -> <<>>;
reverse_key_default(Key) -> Key.


changes_key_opts(StartSeq, Args) ->
    changes_key_opts(StartSeq, Args, []).


changes_key_opts(StartSeq, #mrargs{keys=undefined, direction=Dir}=Args, Extra) ->
    [[{dir, Dir}] ++ changes_skey_opts(StartSeq, Args) ++
     changes_ekey_opts(StartSeq, Args) ++ Extra];
changes_key_opts(StartSeq, #mrargs{keys=Keys, direction=Dir}=Args, Extra) ->
    lists:map(fun(K) ->
        [{dir, Dir}]
        ++ changes_skey_opts(StartSeq, Args#mrargs{start_key=K})
        ++ changes_ekey_opts(StartSeq, Args#mrargs{end_key=K})
        ++ Extra
    end, Keys).


changes_skey_opts(StartSeq, #mrargs{start_key=undefined}) ->
    [{start_key, {-16#ffffffffffffffff, StartSeq+1}}];
changes_skey_opts(StartSeq, #mrargs{start_key=SKey}) ->
    [{start_key, {SKey, StartSeq+1}}].


changes_ekey_opts(_StartSeq, #mrargs{end_key=undefined}) ->
    [];
changes_ekey_opts(_StartSeq, #mrargs{end_key=EKey,
                                     direction=Dir}=Args) ->
    EndSeq = case Dir of
        fwd -> ?SEQ_MAX;
        rev -> 0
    end,

    case Args#mrargs.inclusive_end of
        true -> [{end_key, {EKey, EndSeq}}];
        false -> [{end_key_gt, {EKey, EndSeq}}]
    end.



calculate_data_size(IdBt, Views) ->
    SumFun = fun(#mrview{btree=Bt, seq_btree=SBt, kseq_btree=KSBt}, Acc) ->
        Size0 = sum_btree_sizes(Acc, couch_btree:size(Bt)),
        Size1 = sum_btree_sizes(Size0, couch_btree:size(SBt)),
        sum_btree_sizes(Size1, couch_btree:size(KSBt))
    end,
    Size = lists:foldl(SumFun, couch_btree:size(IdBt), Views),
    {ok, Size}.


sum_btree_sizes(nil, _) ->
    null;
sum_btree_sizes(_, nil) ->
    null;
sum_btree_sizes(Size1, Size2) ->
    Size1 + Size2.


detuple_kvs([], Acc) ->
    lists:reverse(Acc);
detuple_kvs([KV | Rest], Acc) ->
    {{Key,Id},Value} = KV,
    NKV = [[Key, Id], Value],
    detuple_kvs(Rest, [NKV | Acc]).


expand_dups([], Acc) ->
    lists:reverse(Acc);
expand_dups([{Key, {dups, Vals}} | Rest], Acc) ->
    Expanded = [{Key, Val} || {Val, _Seq} <- Vals],
    expand_dups(Rest, Expanded ++ Acc);
expand_dups([{K, {V, _Seq}}| Rest], Acc) ->
    expand_dups(Rest, [{K, V} | Acc]).


changes_expand([], _Type, Acc) ->
    lists:reverse(Acc);
changes_expand([{{Key, Seq}, {Val, DocId}} | Rest], by_key, Acc) ->
    changes_expand(Rest, by_key, [{{Seq, Key, DocId}, Val} | Acc]);
changes_expand([{Seq, {Val, Key, DocId}} | Rest], Type, Acc) ->
    changes_expand(Rest, Type, [{{Seq, Key, DocId}, Val} | Acc]).

maybe_load_doc(_Db, _DI, #mrargs{include_docs=false}) ->
    [];
maybe_load_doc(Db, #doc_info{}=DI, #mrargs{conflicts=true, doc_options=Opts}) ->
    doc_row(barrel_doc:load(Db, DI, [conflicts]), Opts);
maybe_load_doc(Db, #doc_info{}=DI, #mrargs{doc_options=Opts}) ->
    doc_row(barrel_doc:load(Db, DI, []), Opts).


maybe_load_doc(_Db, _Id, _Val, #mrargs{include_docs=false}) ->
    [];
maybe_load_doc(Db, Id, Val, #mrargs{conflicts=true, doc_options=Opts}) ->
    doc_row(barrel_doc:load(Db, docid_rev(Id, Val), [conflicts]), Opts);
maybe_load_doc(Db, Id, Val, #mrargs{doc_options=Opts}) ->
    doc_row(barrel_doc:load(Db, docid_rev(Id, Val), []), Opts).


doc_row(null, _Opts) ->
    [{doc, null}];
doc_row(Doc, Opts) ->
    [{doc, barrel_doc:to_json_obj(Doc, Opts)}].


docid_rev(Id, {Props}) ->
    DocId = maps:get(<<"_id">>, Props, Id),
    Rev = case maps:get(<<"_rev">>, Props, nil) of
        nil -> nil;
        Rev0 -> barrel_doc:parse_rev(Rev0)
    end,
    {DocId, Rev};
docid_rev(Id, _) ->
    {Id, nil}.


index_of(Key, List) ->
    index_of(Key, List, 1).


index_of(_, [], _) ->
    throw({error, missing_named_view});
index_of(Key, [Key | _], Idx) ->
    Idx;
index_of(Key, [_ | Rest], Idx) ->
    index_of(Key, Rest, Idx+1).


mrverror(Mesg) ->
    throw({query_parse_error, Mesg}).


to_key_seq(L) ->
    [{{[Key, Seq], DocId}, {Val, Rev}} || {{Seq, Key}, {DocId, Val, Rev}} <- L].
