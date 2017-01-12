%% Copyright (c) 2017. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_indexer).
-author("Benoit Chesneau").
-behaviour(gen_server).

-export([refresh_index/2]).

-export([start_link/2]).

%% API
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("barrel.hrl").


refresh_index(Indexer, Since) ->
  gen_server:call(Indexer, {refresh_index, Since}, infinity).

start_link(Db, Opts) ->
  gen_server:start_link(?MODULE, [Db, Opts], []).

init([Db, Opts]) ->
  UpdateSeq = last_index_seq(Db),
  IndexChangeSize = maps:get(index_changes_size, Opts, ?DEFAULT_CHANGES_SIZE),

  State = #{
    db => Db#db{indexer=self()}, % we probably don't need to set it, but be consistent
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

fetch_changes(Since, #{ db := Db, index_changes_size := Max}) ->
  Fun = fun
          (_Seq, Change, {N, Acc}) ->
            N2 = N + 1,
            if
              N2 < Max -> {ok, {N2, [Change | Acc]}};
              true -> {stop, {N2, [Change | Acc]}}
            end
        end,
  {NChanges, Changes} = barrel_db:changes_since_int(
    Db, Since, Fun, {0, []}, [{include_doc, true}]
  ),
  lager:debug(
    "~s: fetched ~p changes since ~p:~n~n~p",
    [?MODULE_STRING, NChanges, Since, Changes]
  ),
  lists:reverse(Changes).

process_changes(Changes, State0 = #{ db := Db }) ->
  #{ update_seq := LastSeq } = State2 = lists:foldl(
    fun(Change = #{ seq := Seq }, State = #{ update_seq := OldSeq}) ->
      {ToAdd, ToDel, DocId, FullPaths} = analyze(Change, Db),
      lager:debug(
        "~s: processed changed in ~p, ~n - to add:~n~p~n - to del:~n~p",
        [?MODULE_STRING, Db#db.name, ToAdd, ToDel]
      ),
      ForwardOps = merge_forward_paths(ToAdd, ToDel, DocId, Db),
      ReverseOps = merge_reverse_paths(ToAdd, ToDel, DocId, Db),

      ok = update_index(Db, ForwardOps, ReverseOps, DocId, FullPaths),
      State#{ update_seq => erlang:max(Seq, OldSeq) }
    end,
    State0,
    Changes
  ),
  {{ok, LastSeq}, State2}.


update_index(#db{id=DbId, store=Store}, ForwardOps, ReverseOps, DocId, FullPaths) ->
  Ops = prepare_index(
    ForwardOps, fun barrel_keys:idx_forward_path_key/2, DbId,
    prepare_index(
      ReverseOps, fun barrel_keys:idx_reverse_path_key/2, DbId,
      []
    )
  ),
  case Ops of
    [] -> ok;
    _ ->
      Batch = [
        {put, barrel_keys:idx_last_doc_key(DbId, DocId), term_to_binary(FullPaths)}
      ] ++ Ops,
      rocksdb:write(Store, Batch, [{sync, true}])
  end.

prepare_index([{put, Path, Entries} | Rest], KeyFun, DbId, Acc) ->
  Key = KeyFun(DbId, Path),
  Acc2 = [{put, Key, term_to_binary(Entries)} | Acc],
  prepare_index(Rest, KeyFun, DbId, Acc2);
prepare_index([{delete, Path} | Rest], KeyFun, DbId, Acc) ->
  Key = KeyFun(DbId, Path),
  Acc2 = [{delete, Key} | Acc],
  prepare_index(Rest, KeyFun, DbId, Acc2);
prepare_index([], _KeyFun, _DbId, Acc) ->
  Acc.

last_index_seq(#db{ indexed_seq = Seq}) -> Seq.

get_last_doc(#db{id=DbId, store=Store}, DocId) ->
  case rocksdb:get(Store, barrel_keys:idx_last_doc_key(DbId, DocId), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

get_reverse_path(#db{id=DbId, store=Store}, Path) ->
  case rocksdb:get(Store, barrel_keys:idx_reverse_path_key(DbId, Path), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

get_forward_path(#db{id=DbId, store=Store}, Path) ->
  case rocksdb:get(Store, barrel_keys:idx_forward_path_key(DbId, Path), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

merge_forward_paths(ToAdd, ToDel, DocId, Db) ->
  ToAdd1 = lists:usort([lists:reverse(P) || P <- ToAdd]),
  ToDel1 = lists:usort([lists:reverse(P) || P <- ToDel]),
  Ops0 = merge(ToAdd1,  DocId, add, fun get_forward_path/2, Db, []),
  merge(ToDel1, DocId, del, fun get_forward_path/2, Db, Ops0).

merge_reverse_paths(ToAdd, ToDel, DocId, Db) ->
  Ops0 = merge(ToAdd, DocId, add, fun get_reverse_path/2, Db, []),
  merge(ToDel, DocId, del, fun get_reverse_path/2, Db, Ops0).


merge([Path | Rest], DocId, Op, Fun, Db, Acc) ->
  case Fun(Db, Path) of
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
      merge(Rest, DocId, Op, Fun, Db, Acc2);
    not_found ->
      Acc2 = case Op of
               add ->
                 [{put, Path, [DocId]} | Acc];
               del ->
                 Acc
             end,
      merge(Rest, DocId, Op, Fun, Db, Acc2)
  end;
merge([], _DocId, _Op, _Fun, _Db, Acc) ->
  Acc.

analyze(Change, Db) ->
  Doc = maps:get(doc, Change),
  Del = maps:get(deleted, Change, false),
  DocId = barrel_doc:id(Doc),
  OldPaths = case get_last_doc(Db, DocId) of
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


