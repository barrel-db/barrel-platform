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

-module(couch_mrview_updater).

-export([start_update/3, purge/4, process_doc/3, finish_update/1]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_mrview.hrl").


-define(REM_VAL, removed).


start_update(Partial, State, NumChanges) ->
    QueueOpts = [{max_size, 100000}, {max_items, 500}],
    {ok, DocQueue} = couch_work_queue:new(QueueOpts),
    {ok, WriteQueue} = couch_work_queue:new(QueueOpts),

    InitState = State#mrst{
        first_build=State#mrst.update_seq==0,
        partial_resp_pid=Partial,
        doc_acc=[],
        doc_queue=DocQueue,
        write_queue=WriteQueue
    },

    Self = self(),
    MapFun = fun() ->
        barrel_task_status:add_task([
            {type, indexer},
            {database, State#mrst.db_name},
            {design_document, State#mrst.idx_name},
            {progress, 0},
            {changes_done, 0},
            {total_changes, NumChanges}
        ]),
        barrel_task_status:set_update_frequency(500),
        map_docs(Self, InitState)
    end,
    WriteFun = fun() -> write_results(Self, InitState) end,

    spawn_link(MapFun),
    spawn_link(WriteFun),

    {ok, InitState}.


purge(_Db, PurgeSeq, PurgedIdRevs, State) ->
    #mrst{
        id_btree=IdBtree,
        views=Views
    } = State,
    
    Ids = [Id || {Id, _Revs} <- PurgedIdRevs],
    {ok, Lookups, IdBtree2} = couch_btree:query_modify(IdBtree, Ids, [], Ids),

    MakeDictFun = fun
        ({ok, {DocId, ViewNumRowKeys}}, DictAcc) ->
            FoldFun = fun({ViewNum, {Key, Seq}}, {DK1, DS1, DKS1}) ->
                DK2 = dict:append(ViewNum, {Key, DocId}, DK1),
                DS2 = dict:append(ViewNum, Seq, DS1),
                DKS2 = dict:append(ViewNum, {Key, Seq}, DKS1),
                {DK2, DS2, DKS2}
            end,
            lists:foldl(FoldFun, DictAcc, ViewNumRowKeys);
        ({not_found, _}, DictAcc) ->
            DictAcc
    end,
    DictAcc0 = {dict:new(), dict:new(), dict:new()},
    {KeysToRemove, SeqsToRemove, KSeqsToRemove} = lists:foldl(MakeDictFun, DictAcc0, Lookups),

    RemKeysFun = fun(#mrview{id_num=ViewId}=View) ->
        ToRem = couch_util:dict_find(ViewId, KeysToRemove, []),
        {ok, VBtree2} = couch_btree:add_remove(View#mrview.btree, [], ToRem),
        {NewPurgeSeq, IsPurged} = case VBtree2 =/= View#mrview.btree of
                                      true -> {PurgeSeq, true};
                                      _ -> {View#mrview.purge_seq, false}
                                  end,

        {ok, SeqBtree2, KSeqBtree2} =
        case IsPurged of
            true ->
                SToRem = couch_util:dict_find(ViewId, SeqsToRemove, []),
                KSToRem = couch_util:dict_find(ViewId, KSeqsToRemove, []),
                {ok, SBt} = couch_btree:add_remove(View#mrview.seq_btree, [],
                                                   SToRem),
                {ok, KSBt} = couch_btree:add_remove(View#mrview.kseq_btree, [],
                                                   KSToRem),
                {ok, SBt, KSBt};
            false ->
                {ok, View#mrview.seq_btree, View#mrview.kseq_btree}
        end,

        View#mrview{btree=VBtree2,
                    seq_btree=SeqBtree2,
                    kseq_btree=KSeqBtree2,
                    purge_seq=NewPurgeSeq}

    end,

    Views2 = lists:map(RemKeysFun, Views),
    {ok, State#mrst{
        id_btree=IdBtree2,
        views=Views2,
        purge_seq=PurgeSeq
    }}.


process_doc(Doc, Seq, #mrst{doc_acc=Acc}=State) when length(Acc) > 100 ->
    couch_work_queue:queue(State#mrst.doc_queue, lists:reverse(Acc)),
    process_doc(Doc, Seq, State#mrst{doc_acc=[]});
process_doc(nil, Seq, #mrst{doc_acc=Acc}=State) ->
    {ok, State#mrst{doc_acc=[{nil, Seq, nil, nil} | Acc]}};
process_doc(#doc{id=Id, deleted=true}=Doc, Seq, #mrst{doc_acc=Acc}=State) ->
    Rev= extract_rev(Doc#doc.revs),
    {ok, State#mrst{doc_acc=[{Id, Seq, Rev, deleted} | Acc]}};
process_doc(#doc{id=Id}=Doc, Seq, #mrst{doc_acc=Acc}=State) ->
    Rev = extract_rev(Doc#doc.revs),
    {ok, State#mrst{doc_acc=[{Id, Seq, Rev, Doc} | Acc]}}.

extract_rev({0, []}) ->
    {0, []};
extract_rev({RevPos, [Rev | _]}) ->
    {RevPos, Rev}.

finish_update(#mrst{doc_acc=Acc}=State) ->
    if Acc /= [] ->
        couch_work_queue:queue(State#mrst.doc_queue, Acc);
        true -> ok
    end,
    couch_work_queue:close(State#mrst.doc_queue),
    receive
        {new_state, NewState} ->
            {ok, NewState#mrst{
                first_build=undefined,
                partial_resp_pid=undefined,
                doc_acc=undefined,
                doc_queue=undefined,
                write_queue=undefined,
                qserver=nil
            }}
    end.


map_docs(Parent, State0) ->
    case couch_work_queue:dequeue(State0#mrst.doc_queue) of
        closed ->
            couch_query_servers:stop_doc_map(State0#mrst.qserver),
            couch_work_queue:close(State0#mrst.write_queue);
        {ok, Dequeued} ->
            % Run all the non deleted docs through the view engine and
            % then pass the results on to the writer process.
            State1 = case State0#mrst.qserver of
                nil -> start_query_server(State0);
                _ -> State0
            end,
            QServer = State1#mrst.qserver,
            DocFun = fun
                ({nil, Seq, _, _}, {SeqAcc, Results}) ->
                    {erlang:max(Seq, SeqAcc), Results};
                ({Id, Seq, Rev, deleted}, {SeqAcc, Results}) ->
                    {erlang:max(Seq, SeqAcc), [{Id, Seq, Rev, []} | Results]};
                ({Id, Seq, Rev, Doc}, {SeqAcc, Results}) ->
                    {ok, Res} = couch_query_servers:map_doc_raw(QServer, Doc),
                    {erlang:max(Seq, SeqAcc), [{Id, Seq, Rev, Res} | Results]}
            end,
            FoldFun = fun(Docs, Acc) ->
                update_task(length(Docs)),
                lists:foldl(DocFun, Acc, Docs)
            end,
            Results = lists:foldl(FoldFun, {0, []}, Dequeued),
            couch_work_queue:queue(State1#mrst.write_queue, Results),
            map_docs(Parent, State1)
    end.


write_results(Parent, State) ->
    #mrst{group_seq=GroupSeq, write_queue=WQ, views=Views} = State,
    Acc0 = {0, [{V#mrview.id_num, []} || V <- Views], [], GroupSeq},
    case accumulate_writes(WQ, Acc0) of
        stop ->
            Parent ! {new_state, State};
        {Go, {Seq, ViewKVs, DocIdKeys, GroupSeq2}} ->
            NewState = write_kvs(State, Seq, ViewKVs, DocIdKeys, GroupSeq2),
            if Go == stop ->
                Parent ! {new_state, NewState};
            true ->
                send_partial(NewState#mrst.partial_resp_pid, NewState),
                write_results(Parent, NewState)
            end
    end.


start_query_server(State) ->
    #mrst{
        language=Language,
        lib=Lib,
        views=Views
    } = State,
    Defs = [View#mrview.def || View <- Views],
    {ok, QServer} = couch_query_servers:start_doc_map(Language, Defs, Lib),
    State#mrst{qserver=QServer}.



accumulate_writes(W, Acc0) ->
    {Seq, ViewKVs, DocIdKVs, GroupSeq} = Acc0,
    case couch_work_queue:dequeue(W) of
        closed when Seq == 0 ->
            stop;
        closed ->
            {stop, {Seq, ViewKVs, DocIdKVs, GroupSeq}};
        {ok, Info} ->
            {_, _, NewIds, _} = Acc = merge_results(Info, Seq, ViewKVs,
                                                    DocIdKVs, GroupSeq),
            case accumulate_more(length(NewIds)) of
                true -> accumulate_writes(W, Acc);
                false -> {ok, Acc}
            end
    end.


accumulate_more(NumDocIds) ->
    % check if we have enough items now
    MinItems = barrel_config:get_integer("view_updater", "min_writer_items", 100),
    MinSize = barrel_config:get_integer("view_updater", "min_writer_size", 16777216),
    {memory, CurrMem} = process_info(self(), memory),
    NumDocIds < MinItems andalso CurrMem < MinSize.


merge_results([], SeqAcc, ViewKVs, DocIdKeys, GroupSeq) ->
    {SeqAcc, ViewKVs, DocIdKeys, GroupSeq};
merge_results([{Seq, Results} | Rest], SeqAcc, ViewKVs, DocIdKeys, GroupSeq) ->
    Fun = fun(RawResults, {VKV, DIK, GroupSeq2}) ->
        merge_results(RawResults, VKV, DIK, GroupSeq2)
    end,
    {ViewKVs1, DocIdKeys1, GroupSeq1} = lists:foldl(Fun, {ViewKVs, DocIdKeys, GroupSeq}, Results),
    merge_results(Rest, erlang:max(Seq, SeqAcc), ViewKVs1, DocIdKeys1, GroupSeq1).


merge_results({DocId, _Seq, _Rev, []}, ViewKVs, DocIdKeys, GroupSeq) ->
    {ViewKVs, [{DocId, []} | DocIdKeys], GroupSeq};
merge_results({DocId, Seq, Rev, RawResults}, ViewKVs, DocIdKeys, GroupSeq) ->
    JsonResults = couch_query_servers:raw_to_ejson(RawResults),
    Results = [[list_to_tuple(Res) || Res <- FunRs] || FunRs <- JsonResults],
    case lists:flatten(Results) of
        [] ->
            {ViewKVs, [{DocId, []} | DocIdKeys], GroupSeq};
        _ ->
            {ViewKVs1, ViewIdKeys, GroupSeq1} = insert_results(DocId, Seq, Rev,
                                                          Results, ViewKVs,
                                                          [], [], GroupSeq),
            {ViewKVs1, [ViewIdKeys | DocIdKeys], GroupSeq1}
    end.


insert_results(DocId, _Seq, _Rev, [], [], ViewKVs, ViewIdKeys, GroupSeq) ->
    {lists:reverse(ViewKVs), {DocId, ViewIdKeys}, GroupSeq};
insert_results(DocId, Seq, Rev, [KVs | RKVs], [{Id, VKVs} | RVKVs], VKVAcc,
               VIdKeys, GroupSeq) ->
    CombineDupesFun = fun
        ({Key, Val}, {[{Key, {dups, Vals}} | Rest], IdKeys, GroupSeq1}) ->
            GroupSeq3 = GroupSeq1 + 1,
            {[{Key, {dups, [{Val, GroupSeq3} | Vals]}} | Rest], IdKeys, GroupSeq3};
        ({Key, Val1}, {[{Key, Val2} | Rest], IdKeys, GroupSeq1}) ->
            GroupSeq3 = GroupSeq1 + 2,
            Dups = [{Val1, GroupSeq3}, Val2],
            {[{Key, {dups, Dups}} | Rest], IdKeys, GroupSeq3};
        ({Key, Value}, {Rest, IdKeys, GroupSeq1}) ->
            GroupSeq3 = GroupSeq1 + 1,
            {[{Key, {Value, GroupSeq3}} | Rest], [{Id, {Key, GroupSeq3}} | IdKeys], GroupSeq3}
    end,
    InitAcc = {[], VIdKeys, GroupSeq},
    {Duped, VIdKeys0, GroupSeq2} = lists:foldl(CombineDupesFun, InitAcc,
                                               lists:sort(KVs)),
    FinalKVs = [{{Key, DocId}, Val} || {Key, Val} <- Duped] ++ VKVs,
    insert_results(DocId, Seq, Rev, RKVs, RVKVs,
                  [{Id, FinalKVs} | VKVAcc], VIdKeys0, GroupSeq2).

write_kvs(State, UpdateSeq, ViewKVs, DocIdKeys, GroupSeq0) ->
    #mrst{
        id_btree=IdBtree,
        first_build=FirstBuild
    } = State,

    {ok, RemByView, GroupSeq, IdBtree2} = update_id_btree(IdBtree, DocIdKeys,
                                                          GroupSeq0, FirstBuild),


    UpdateView = fun(#mrview{id_num=ViewId}=View, {ViewId, KVs}) ->
        RemKVs = couch_util:dict_find(ViewId, RemByView, []),
        ToAdd = KVs ++ RemKVs,
        {ToFind, SKVs, KSKVs, Seqs} =
        lists:foldl(fun
            ({{Key, DocId}=K, {dups, Vals}}, {Keys1, SKVs1, KSKVs1, Seqs1}) ->
                            lists:foldl(fun({Val, Seq}, {Keys3, SKVs3, KSKVs3, Seqs3}) ->
                                                SRow = {Seq, {Val, Key, DocId}},
                                                KSRow = {{Key, Seq}, {Val, DocId}},
                                                SKVs2 = [SRow | SKVs3],
                                                KSKVs2 = [KSRow | KSKVs3],
                                                Keys2 = [K | Keys3],
                                                Seqs2 = [Seq | Seqs3],
                                                {Keys2, SKVs2, KSKVs2, Seqs2}
                                    end, {Keys1, SKVs1, KSKVs1, Seqs1}, Vals);
            ({{Key, DocId}=K, {Val, Seq}}, {Keys1, SKVs1, KSKVs1, Seqs1}) ->
                            SRow = {Seq, {Val, Key, DocId}},
                            KSRow = {{Key, Seq}, {Val, DocId}},
                            SKVs2 = [SRow | SKVs1],
                            KSKVs2 = [KSRow | KSKVs1],
                            Keys2 = [K | Keys1],
                            Seqs2 = [Seq | Seqs1],
                            {Keys2, SKVs2, KSKVs2, Seqs2}
                    end, {[], [], [], []}, ToAdd),

        %% update the key tree and retrieved the old results
        {ok, RemovedKeys, VBtree2} = couch_btree:query_modify(View#mrview.btree,
                                                              ToFind, ToAdd, []),
        {NewUpdateSeq, NewGroupSeq, IsUpdated} =
        case VBtree2 =/= View#mrview.btree of
            true -> {UpdateSeq, lists:max(Seqs), true};
            _ -> {View#mrview.update_seq, View#mrview.group_seq, false}
        end,

        {ok, VSeqBtree2, VKSeqBtree2} = case IsUpdated of
            true ->
                {SToDel, KSToDel} = removed_seqs(RemovedKeys, [], []),
                barrel_log:debug("indexing seqs, view ~p~n - add: ~p~n - rem:~p~n", [ViewId, SKVs, SToDel]),
                {ok, SBt} = couch_btree:add_remove(View#mrview.seq_btree, SKVs,
                                                   SToDel),

                {ok, KSBt} = couch_btree:add_remove(View#mrview.kseq_btree, KSKVs,
                                                    KSToDel),
                {ok, SBt, KSBt};
            false ->
                barrel_log:debug("no seq index to update", []),
                {ok, View#mrview.seq_btree, View#mrview.kseq_btree}
        end,

        View#mrview{btree=VBtree2,
                    seq_btree=VSeqBtree2,
                    kseq_btree=VKSeqBtree2,
                    update_seq=NewUpdateSeq,
                    group_seq=NewGroupSeq}
    end,

    State#mrst{
        views=lists:zipwith(UpdateView, State#mrst.views, ViewKVs),
        update_seq=UpdateSeq,
        group_seq=GroupSeq,
        id_btree=IdBtree2
    }.

update_id_btree(Btree, DocIdKeys, Seq, true) ->
    ToAdd = [{Id, DIKeys} || {Id, DIKeys} <- DocIdKeys, DIKeys /= []],
    {ok, _, Btree2} = couch_btree:query_modify(Btree, [], ToAdd, []),
    {ok, dict:new(), Seq, Btree2};
update_id_btree(Btree, DocIdKeys, Seq, _) ->
    %% mark removed keys as removed
    ToFind = [Id || {Id, _} <- DocIdKeys],
    Added = [{Id, DIKeys} || {Id, DIKeys} <- DocIdKeys, DIKeys /= []],
    ToUpdate = couch_btree:lookup(Btree, ToFind),
    {ToAdd, RemByView, NewSeq} = insert_removed(lists:sort(ToUpdate),
                                                lists:sort(Added), Seq,
                                                {[], dict:new()}),

    %% insert new enrtries and mark old entries as removed
    {ok, Btree2} = couch_btree:add_remove(Btree, ToAdd, []),
    {ok, RemByView, NewSeq, Btree2}.

insert_removed([{ok, {DocId, DIK1}} | R1], [{DocId, DIK2} | R2], Seq, {Acc, Dict}) ->
    ToAdd = [{VId, Key} || {VId, {Key, _Seq}} <- DIK2],
    ToUp = [{VId, Key} || {VId, {Key, _Seq}} <- DIK1],
    ToMark = ToUp -- ToAdd,
    {ToRem, NDict, NewSeq} = lists:foldl(fun({VId, Key}, {RemAcc, RemDict1, Seq1}) ->
                                                Seq2 = Seq1 +1,
                                                Row = {{Key, DocId}, {removed, Seq2}},
                                                RemDict2 = dict:append(VId, Row, RemDict1),
                                                RemAcc2 = [{VId, {Key, Seq2}} |RemAcc],
                                                {RemAcc2, RemDict2, Seq2}
                                        end, {[], Dict, Seq}, ToMark),
    DocIdKeys = DIK2 ++ ToRem,
    insert_removed(R1, R2, NewSeq, {[{DocId, DocIdKeys} | Acc], NDict});
insert_removed([{ok, {DocId1, _DIK1}} | _]=R1, [{DocId2, _DIK2}=KV | R2], Seq, {Acc, Dict})
  when DocId2 < DocId1 ->
    insert_removed(R1, R2, Seq, {[KV | Acc], Dict});
insert_removed([{ok, {DocId, DIK1}} | R1], R2, Seq, {Acc, Dict}) ->
    {ToRem, NDict, NewSeq} = lists:foldl(fun({VId, {Key, _Seq}}, {RemAcc, RemDict1, Seq1}) ->
                                          Seq2 = Seq1 +1,
                                          Row = {{Key, DocId}, {removed, Seq2}},
                                          RemAcc2 = [{VId, {Key, Seq2}} | RemAcc],
                                          RemDict2 = dict:append(VId, Row,
                                                                 RemDict1),
                                          {RemAcc2, RemDict2, Seq2}
                                  end, {[], Dict, Seq}, DIK1),
    insert_removed(R1, R2, NewSeq, {[{DocId, ToRem} | Acc], NDict});
insert_removed([not_found| R1], R2, Seq, Acc) ->
    insert_removed(R1, R2, Seq, Acc);
insert_removed([], R2, Seq, {Acc, Dict}) ->
    {R2 ++ Acc, Dict, Seq};
insert_removed(R1, [], Seq, {Acc, Dict}) ->
    lists:foldl(fun({ok, {DocId, DIKeys}}, {Acc1, Dict1, Seq1}) ->
                        {ToRem, NDict, NewSeq} = lists:fold(fun({VId, {Key, _Seq}},
                                                          {Acc2, Dict2, Seq2}) ->
                                           Seq3 = Seq2 + 1,
                                           Row = {{Key, DocId}, {removed, Seq3}},
                                           Acc3 = [{VId, {Key, Seq3}} | Acc2],
                                           Dict3 = dict:append(VId, Row,
                                                               Dict2),
                                           {Acc3, Dict3, Seq3}
                                   end, {[], Dict1, Seq1}, DIKeys),
                        {[{DocId, ToRem} | Acc1], NDict, NewSeq};
                   ({not_found, _}, {Acc1, Dict1, Seq1}) ->
                        {Acc1, Dict1, Seq1}
                end, {Acc, Dict, Seq}, R1).

removed_seqs([], Acc, KAcc) ->
    {Acc, KAcc};
removed_seqs([{ok, {{Key, _DocId}, {_Val, Seq}}} | Rest], Acc, KAcc) ->
    removed_seqs(Rest, [Seq | Acc], [{Key, Seq} | KAcc]);
removed_seqs([_ | Rest], Acc, KAcc) ->
    removed_seqs(Rest, Acc, KAcc).

send_partial(Pid, State) when is_pid(Pid) ->
    gen_server:cast(Pid, {new_state, State});
send_partial(_, _) ->
    ok.


update_task(NumChanges) ->
    [Changes, Total] = barrel_task_status:get([changes_done, total_changes]),
    Changes2 = Changes + NumChanges,
    Progress = case Total of
        0 ->
            % updater restart after compaction finishes
            0;
        _ ->
            (Changes2 * 100) div Total
    end,
    barrel_task_status:update([{progress, Progress}, {changes_done, Changes2}]).
