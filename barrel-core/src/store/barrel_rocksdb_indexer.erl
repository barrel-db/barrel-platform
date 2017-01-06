%% Copyright 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_rocksdb_indexer).
-author("Benoit Chesneau").
-behaviour(gen_server).

-export([refresh_index/2]).

-export([start_link/4]).

%% API
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(DEFAULT_CHANGES_SIZE, 10).


refresh_index(Indexer, Since) ->
  gen_server:call(Indexer, {refresh_index, Since}, infinity).

start_link(Parent, Name, Ref, Opts) ->
  gen_server:start_link(?MODULE, [Parent, Name, Ref, Opts], []).

init([Parent, Name, Ref, Opts]) ->
  {ok, UpdateSeq} = last_index_seq(Ref),
  IndexChangeSize = maps:get(index_changes_size, Opts, ?DEFAULT_CHANGES_SIZE),

  State = #{
    parent => Parent,
    name => Name,
    ref => Ref,
    update_seq => UpdateSeq,
    index_changes_size => IndexChangeSize
  },
  self() ! refresh_index,
  {ok, State}.

handle_call({refresh_index, Since}, _From, State) ->
  {Reply, NState} = do_refresh_index(Since, State),
  {reply, Reply, NState};

handle_call(Req, _From, State) ->
  {reply, {badcall, Req}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(refresh_index, State = #{ update_seq := LastSeq }) ->
  {_Reply, NState} = do_refresh_index(LastSeq, State),
  {noreply, NState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

do_refresh_index(Since, State) ->
  Changes= fetch_changes(Since, State),
  process_changes(Changes, State).

fetch_changes(Since, #{ ref := Ref, index_changes_size := Max}) ->
  Fun = fun
          (_Seq, Change, {N, Acc}) ->
            N2 = N + 1,
            if
              N2 < Max -> {ok, {N2, [Change | Acc]}};
              true -> {stop, {N2, [Change | Acc]}}
            end
        end,
  {NChanges, Changes} = barrel_rocksdb:changes_since(
    {ref, Ref}, Since, Fun, {Since, []}, [{include_doc, true}]
  ),
  lager:debug("fetched ~p changes since ~p~n", [NChanges, Since]),
  lists:reverse(Changes).

process_changes(Changes, State0 = #{ ref := Ref }) ->
  #{ update_seq := LastSeq } = State2 = lists:foldl(
    fun(Change = #{ seq := Seq }, State = #{ update_seq := OldSeq}) ->
      {ToAdd, ToDel, DocId, FullPaths} = analyze(Change, Ref),

      ForwardOps = merge_forward_paths(ToAdd, ToDel, DocId, State),
      ReverseOps = merge_reverse_paths(ToAdd, ToDel, DocId, State),

      ok = update_index(Ref, ForwardOps, ReverseOps, DocId, FullPaths),
      State#{ update_seq => erlang:max(Seq, OldSeq) }
    end,
    State0,
    Changes
  ),
  {{ok, LastSeq}, State2}.


update_index(Ref, ForwardOps, ReverseOps, DocId, FullPaths) ->
  Ops = prepare_index(
    ForwardOps, fun barrel_rocksdb:idx_forward_path_key/1,
    prepare_index(
      ReverseOps, fun barrel_rocksdb:idx_reverse_path_key/1,
      []
    )
  ),
  case Ops of
    [] -> ok;
    _ ->
      Batch = [
        {put, barrel_rocksdb:idx_last_doc_key(DocId), term_to_binary(FullPaths)}
      ] ++ Ops,
      rocksdb:write(Ref, Batch, [{sync, true}])
  end.

prepare_index([{put, Path, Entries} | Rest], KeyFun, Acc) ->
  Key = KeyFun(Path),
  Acc2 = [{put, Key, term_to_binary(Entries)} | Acc],
  prepare_index(Rest, KeyFun, Acc2);
prepare_index([{delete, Path} | Rest], KeyFun, Acc) ->
  Key = KeyFun(Path),
  Acc2 = [{delete, Key} | Acc],
  prepare_index(Rest, KeyFun, Acc2);
prepare_index([], _KeyFun, Acc) ->
  Acc.

last_index_seq(Ref) ->
  case rocksdb:get(Ref, barrel_rocksdb:meta_key(0), []) of
    {ok, BinInfo } ->
      #{ last_index_seq := Seq} = binary_to_term(BinInfo),
      {ok, Seq};
    not_found -> {ok, 0}; % race condition?
    Error -> Error
  end.

index_get_last_doc(Ref, DocId) ->
  case rocksdb:get(Ref, barrel_rocksdb:idx_last_doc_key(DocId), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

index_get_reverse_path(Ref, Path) ->
  case rocksdb:get(Ref, barrel_rocksdb:idx_reverse_path_key(Path), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

index_get_forward_path(Ref, Path) ->
  case rocksdb:get(Ref, barrel_rocksdb:idx_forward_path_key(Path), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.


merge_forward_paths(ToAdd, ToDel, DocId, St) ->
  ToAdd1 = lists:usort([lists:reverse(P) || P <- ToAdd]),
  ToDel1 = lists:usort([lists:reverse(P) || P <- ToDel]),
  Ops0 = merge(ToAdd1,  DocId, add, fun index_get_forward_path/2, St, []),
  merge(ToDel1, DocId, del, fun index_get_forward_path/2, St, Ops0).

merge_reverse_paths(ToAdd, ToDel, DocId, St) ->
  Ops0 = merge(ToAdd, DocId, add, fun index_get_reverse_path/2, St, []),
  merge(ToDel, DocId, del, fun index_get_reverse_path/2, St, Ops0).


merge([Path | Rest], DocId, Op, Fun, St = #{ ref := Ref }, Acc) ->
  case Fun(Ref, Path) of
    {ok, Entries} ->
      Entries2 = case Op of
                   add -> lists:usort([DocId | Entries]) ;
                   del -> Entries -- [DocId]
                 end,
      Sz = length(Entries2),
      Acc2 = if
               Sz > 0 ->
                 [{put, Path, Entries2} | Acc];
               true ->
                 [{delete, Path} | Acc]
             end,
      merge(Rest, DocId, Op, Fun, St, Acc2);
    not_found ->
      Acc2 = case Op of
               add ->
                 [{put, Path, [DocId]} | Acc];
               del ->
                 Acc
             end,
      merge(Rest, DocId, Op, Fun, St, Acc2)
  end;
merge([], _DocId, _Op, _Fun, _St, Acc) ->
  Acc.

analyze(Change, Ref) ->
  Doc = maps:get(doc, Change),
  Del = maps:get(deleted, Change, false),
  DocId = barrel_doc:id(Doc),
  OldPaths = case index_get_last_doc(Ref, DocId) of
               {ok, OldPaths1} -> OldPaths1;
               not_found -> []
             end,
  case Del of
    true ->
      {[], OldPaths, DocId, []};
    false ->
      Paths = barrel_json:flatten(Doc),
      Removed = OldPaths -- Paths,
      Added = Paths -- OldPaths,
      {Added, Removed, DocId, Paths}
  end.


