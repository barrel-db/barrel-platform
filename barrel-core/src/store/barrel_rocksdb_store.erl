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

%% Created by benoitc on 03/09/16.

-module(barrel_rocksdb_store).
-author("Benoit Chesneau").

-behaviour(barrel_store).

-include("barrel.hrl").

%% API
-export([
  init/2,
  open_db/3,
  clean_db/3,
  all_dbs/1,
  get_doc_info/3, get_doc_info/4,
  write_doc/6,
  get_doc/7,
  fold_by_id/5,
  changes_since/6,
  last_update_seq/2,
  write_system_doc/4,
  read_system_doc/3,
  delete_system_doc/3
]).


-export([
  index_seq/2,
  index_get_forward_path/3,
  index_get_reverse_path/3,
  index_get_last_doc/3,
  update_index/7,
  find_by_key/6
]).

-define(VERSION, 1).

init(_Name, #{ store_backend := Backend }) ->
  Db = barrel_rocksdb_backend:get_db(Backend),
  {ok, #{db => Db }}.

open_db(#{ db := Db }, Name, Options) ->
  CreateIfMissing = proplists:get_value(create_if_missing, Options, false),
  DbKey = << 0, 0, 0, 100, (barrel_lib:to_binary(Name))/binary >>,
  case erocksdb:get(Db, DbKey, []) of
    {ok, DbId} ->
      UpdateSeq = get_update_seq(Db, DbId),
      {ok, {DbId, UpdateSeq}};
    not_found when CreateIfMissing /= false  ->
      DbId = << (barrel_lib:uniqid())/binary, "-", Name/binary >>,
      Batch =  [
                 {put, << 0, 0, 0, 0 >>, integer_to_binary(?VERSION)},
                 {put, DbKey, DbId},
                 {put, meta_key(DbId, 0), integer_to_binary(0)}
      ],
      ok = erocksdb:write(Db,Batch, [{sync, true}]),
      {ok, {DbId, 0}};
    not_found ->
      {error, not_found};
    Error ->
      Error
  end.

clean_db(Name, DbId, #{db := Db}) ->
  DbKey = << 0, 0, 0, 100, (barrel_lib:to_binary(Name))/binary >>,
  ok = erocksdb:delete(Db, DbKey, [{sync, true}]),
  _ = spawn(fun() ->
          fold_prefix(Db, DbId, fun clean_db_fun/3, {Db, []})
        end),
  ok.

clean_db_fun(K, _V, {Db, Acc}) ->
  Acc2 = [{delete, K} | Acc],
  Sz = length(Acc2),
  if
    Sz >= 500 ->
      _ = erocksdb:write(Db, Acc2, []),
      {ok, {Db, []}};
    true ->
      {ok, {Db, Acc2}}
  end.

all_dbs(#{db := Db}) ->
  Fun = fun
          ( << 0, 0, 0, 100, Name/binary >>, _, Acc) -> {ok, [Name | Acc]};
          (_, _, _Acc) -> stop
        end,
  AllDbs = fold_prefix(Db, << 0, 0, 0, 100 >>, Fun, []),
  lists:usort(AllDbs).

fold_prefix(Db, Prefix, Fun, AccIn) ->
  fold_prefix(Db, Prefix, Fun, AccIn, []).

fold_prefix(Db, Prefix, Fun, AccIn, Opts) ->
  ReadOptions = proplists:get_value(read_options, Opts, []),

  {ok, Itr} = erocksdb:iterator(Db, ReadOptions),
  try do_fold_prefix(Itr, Prefix, Fun, AccIn, barrel_lib:parse_fold_options(Opts))
  after erocksdb:iterator_close(Itr)
  end.

do_fold_prefix(Itr, Prefix, Fun, AccIn, Opts = #{ gt := GT, gte := GTE}) ->
  {Start, Inclusive} = case {GT, GTE} of
                         {nil, nil} -> {Prefix, true};
                         {first, _} -> {Prefix, false};
                         {_, first} -> {Prefix, true};
                         {_, K} when is_binary(K) ->
                           FirstKey = << Prefix/binary, K/binary >>,
                           {FirstKey, true};
                         {K, _} when is_binary(K) ->
                           FirstKey = << Prefix/binary, K/binary >>,
                           {FirstKey, false};
                         _ ->
                           error(badarg)
                       end,
  Opts2 = Opts#{prefix => Prefix},
  case erocksdb:iterator_move(Itr, Start) of
    {ok, Start, _V} when Inclusive /= true ->
      fold_prefix_loop(erocksdb:iterator_move(Itr, next), Itr, Fun, AccIn, 0, Opts2);
    Next ->
      fold_prefix_loop(Next, Itr, Fun, AccIn, 0, Opts2)
  end.

fold_prefix_loop({error, iterator_closed}, _Itr, _Fun, Acc, _N, _Opts) ->
  throw({iterator_closed, Acc});
fold_prefix_loop({error, invalid_iterator}, _Itr, _Fun, Acc, _N, _Opts) ->
  Acc;

fold_prefix_loop({ok, K, _V}=KV, Itr, Fun, Acc, N0,
                 Opts = #{ lt := Lt, lte := nil, prefix := Prefix})
  when Lt =:= nil orelse K < <<Prefix/binary, Lt/binary>> ->
  fold_prefix_loop1(KV, Itr, Fun, Acc, N0, Opts);


fold_prefix_loop({ok, K, _V}=KV, Itr, Fun, Acc, N,
                 Opts = #{ lt := nil, lte := Lte, prefix := Prefix})
  when Lte =:= nil orelse K =< <<Prefix/binary, Lte/binary>> ->
  fold_prefix_loop1(KV, Itr, Fun, Acc, N, Opts);


fold_prefix_loop({ok, K, V}, _Itr, Fun, Acc, _N,  #{ lt := nil, lte := K, prefix := P}) ->
  case match_prefix(K, P) of
    true ->
      case Fun(K, V, Acc) of
        {ok, Acc2} -> Acc2;
        {stop, Acc2} -> Acc2;
        stop -> Acc
      end;
    false ->
      Acc
  end;
fold_prefix_loop(_KV, _Itr, _Fun, Acc, _N, _Opts) ->
  Acc.

fold_prefix_loop1({ok, K, V}, Itr, Fun, Acc0, N0, Opts) ->
  #{max := Max, prefix := P} = Opts,
  N = N0 + 1,
  case match_prefix(K, P) of
    true ->
      case Fun(K, V, Acc0) of
        {ok, Acc} when (Max =:= 0) orelse (N < Max) ->
          fold_prefix_loop(erocksdb:iterator_move(Itr, next),
                           Itr, Fun, Acc, N, Opts);
        {ok, Acc} -> Acc;
        stop -> Acc0;
        {stop, Acc} -> Acc
      end;
    false ->
      Acc0
  end.

match_prefix(Bin, Prefix) ->
  L = byte_size(Prefix),
  case Bin of
    << Prefix:L/binary, _/binary >> -> true;
    _ -> false
  end.

get_doc_info(DbId, DocId, State) ->
  get_doc_info(DbId, DocId, State, []).

get_doc_info(DbId, DocId,  #{ db := Db}, ReadOptions) ->
  get_doc_info(DbId, DocId,  Db, ReadOptions);
get_doc_info(DbId, DocId,  Db, ReadOptions) ->
  DocKey = doc_key(DbId, DocId),
  case erocksdb:get(Db, DocKey, ReadOptions) of
    {ok, BinDocInfo} -> {ok, binary_to_term(BinDocInfo)};
    not_found -> {error, not_found}
  end.

get_update_seq(Db, DbId) ->
  {ok, SeqBin} = erocksdb:get(Db, meta_key(DbId, 0), []),
  binary_to_integer(SeqBin).

write_doc(DbId, DocId, LastSeq, DocInfo, Body, #{ db := Db}) ->
  #{update_seq := Seq} = DocInfo,
  #{<<"_rev">> := Rev} = Body,
  DocInfoBin = term_to_binary(DocInfo),
  Batch = [
    {put, rev_key(DbId, DocId, Rev), term_to_binary(Body)},
    {put, doc_key(DbId, DocId), DocInfoBin},
    {put, seq_key(DbId, Seq), DocInfoBin},
    {put, meta_key(DbId, 0), barrel_lib:to_binary(Seq)}
  ] ++ case LastSeq of
         undefined -> [];
         _ -> [{delete, seq_key(DbId, LastSeq)}]
       end,
  erocksdb:write(Db, Batch, [{sync, true}]).


get_doc(DbId, DocId, Rev, WithHistory, MaxHistory, HistoryFrom, #{ db := Db}) ->
  {ok, Snapshot} = erocksdb:snapshot(Db),
  ReadOptions = [{snapshot, Snapshot}],

  try get_doc1(DbId, DocId, Db, Rev, WithHistory, MaxHistory, HistoryFrom, ReadOptions)
  after erocksdb:release_snapshot(Snapshot)
  end.

get_doc1(DbId, DocId, Db, Rev, WithHistory, MaxHistory, Ancestors, ReadOptions) ->
  case get_doc_info(DbId, DocId, Db, ReadOptions) of
    {ok, #{revtree := RevTree} = DocInfo} ->
      RevId = case Rev of
              <<"">> -> maps:get(current_rev, DocInfo);
              UserRev -> UserRev
            end,

      case get_doc_rev(Db, DbId, DocId, RevId, ReadOptions) of
        {ok, #{ <<"_deleted">> := true }} when Rev =:= <<"">> ->
          {error, not_found};
        {ok, Doc} ->
          case WithHistory of
            true ->
              History = barrel_revtree:history(RevId, RevTree),
              EncodedRevs = barrel_doc:encode_revisions(History),
              Revisions = barrel_doc:trim_history(EncodedRevs, Ancestors, MaxHistory),
              {ok, Doc#{<<"_revisions">> => Revisions}};
            false ->
              {ok, Doc}
          end;
        Error -> Error
      end;
    Error ->  Error
  end.


get_doc_rev(Db, DbId, DocId, RevId, ReadOptions) ->
  case erocksdb:get(Db, rev_key(DbId, DocId, RevId), ReadOptions) of
    {ok, Bin} -> {ok, binary_to_term(Bin)};
    not_found -> {error, not_found};
    Error -> Error
  end.

fold_by_id(DbId, Fun, AccIn, Opts, #{ db := Db}) ->
  Prefix = << DbId/binary, 0, 0, 50 >>,
  {ok, Snapshot} = erocksdb:snapshot(Db),
  ReadOptions = [{snapshot, Snapshot}],
  IncludeDoc = proplists:get_value(include_doc, Opts, false),
  Opts2 = [{read_options, ReadOptions} | Opts],

  WrapperFun = fun(_Key, BinDocInfo, Acc) ->
    DocInfo = binary_to_term(BinDocInfo),
    RevId = maps:get(current_rev, DocInfo),
    DocId = maps:get(id, DocInfo),
    Doc = case IncludeDoc of
            true ->
              get_doc_rev(Db, DbId, DocId, RevId, ReadOptions);
            false -> {ok, nil}
          end,

    Fun(DocId, DocInfo, Doc, Acc)
  end,

  try fold_prefix(Db, Prefix, WrapperFun, AccIn, Opts2)
  after erocksdb:release_snapshot(Snapshot)
  end.

changes_since(DbId, Since, Fun, AccIn, Opts, #{ db := Db}) ->
  Prefix = << DbId/binary, 0, 0, 100 >>,
  {ok, Snapshot} = erocksdb:snapshot(Db),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOpts = [
    {start_key, <<Since:32>>},
    {read_options, ReadOptions}
  ],
  IncludeDoc = proplists:get_value(include_doc, Opts, false),
  WithHistory = proplists:get_value(history, Opts, last) =:= all,
  WithRevtree =  proplists:get_value(revtree, Opts, false) =:= true,

  WrapperFun =
    fun(Key, BinDocInfo, Acc) ->
      DocInfo = binary_to_term(BinDocInfo),
      [_, SeqBin] = binary:split(Key, Prefix),
      <<Seq:32>> = SeqBin,
      RevId = maps:get(current_rev, DocInfo),
      DocId = maps:get(id, DocInfo),
      RevTree = maps:get(revtree, DocInfo),

      Changes = case WithHistory of
                  false -> [RevId];
                  true ->  barrel_revtree:history(RevId, RevTree)
                end,

      %% create change
      Change = change_with_revtree(
        change_with_doc(
          changes_with_deleted(
            #{ id => DocId, seq => Seq, changes => Changes}, RevId, RevTree
          ),
          DocId, RevId, DbId, Db, ReadOptions, IncludeDoc
        ),
        RevTree,
        WithRevtree
      ),
      Fun(Seq, Change, Acc)
   end,

  try fold_prefix(Db, Prefix, WrapperFun, AccIn, FoldOpts)
  after erocksdb:release_snapshot(Snapshot)
  end.

change_with_revtree(Change, DocInfo, true) -> Change#{revtree => maps:get(revtree, DocInfo)};
change_with_revtree(Change, _DocInfo, false) -> Change.

change_with_doc(Change, DocId, RevId, DbId, Db, ReadOptions, true) ->
  case get_doc_rev(Db, DbId, DocId, RevId, ReadOptions) of
    {ok, Doc} -> Change#{ doc => Doc };
    not_found -> Change#{ doc => {error, missing} }
  end;

change_with_doc(Change, _DocId, _RevId, _DbId, _Db, _ReadOptions, false) ->
  Change.

changes_with_deleted(Change, RevId, RevTree) ->
  {ok, RevInfo} = barrel_revtree:info(RevId, RevTree),
  case RevInfo of
    #{ deleted := true} -> Change#{deleted => true};
    _ -> Change
  end.


last_update_seq(DbId, #{db := Db}) -> get_update_seq(Db, DbId).

%% system storage

write_system_doc(DbId, DocId, Doc, #{db := Db}) ->
  Batch = [{put, sys_key(DbId, DocId), term_to_binary(Doc)}],
  erocksdb:write(Db, Batch, [{sync, true}]).

read_system_doc(DbId, DocId, #{db := Db}) ->
  case erocksdb:get(Db, sys_key(DbId, DocId), []) of
    {ok, Bin} -> {ok, binary_to_term(Bin)};
    not_found -> {error, not_found};
    Error -> Error
  end.

delete_system_doc(DbId, DocId, #{db := Db}) ->
  erocksdb:delete(Db, sys_key(DbId, DocId), [{sync, true}]).


%% index functions

index_seq(DbId, #{ db := Db}) ->
  case erocksdb:get(Db, meta_key(DbId, index_seq), []) of
    {ok, BinVal} -> binary_to_term(BinVal);
    not_found -> 0;
    Error -> Error
  end.

index_get_last_doc(DbId, DocId, #{ db := Db}) ->
  case erocksdb:get(Db, idx_last_doc_key(DbId, DocId), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

index_get_reverse_path(DbId, Path, #{ db := Db}) ->
  case erocksdb:get(Db, idx_reverse_path_key(DbId, Path), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

index_get_forward_path(DbId, Path, #{ db := Db}) ->
  case erocksdb:get(Db, idx_forward_path_key(DbId, Path), []) of
    {ok, BinVal} -> {ok, binary_to_term(BinVal)};
    Error -> Error
  end.

update_index(DbId, ForwardOps, ReverseOps, DocId, Seq, FullPaths, #{ db := Db}) ->
  Ops = prepare_index(
    ForwardOps, DbId, fun idx_forward_path_key/2,
    prepare_index(
      ReverseOps, DbId, fun idx_reverse_path_key/2, []
    )
  ),
  
  case Ops of
    [] -> ok;
    _ ->
      Batch = [
        {put, meta_key(DbId, index_seq), term_to_binary(Seq)},
        {put, idx_last_doc_key(DbId, DocId), term_to_binary(FullPaths)}
      ] ++ Ops,
      erocksdb:write(Db, Batch, [{sync, true}])
  end.

prepare_index([{Op, Path, Entries} | Rest], DbId, KeyFun, Acc) ->
  Key = KeyFun(DbId, Path),
  Acc2 = [{Op, Key, term_to_binary(Entries)} | Acc],
  prepare_index(Rest, DbId, KeyFun, Acc2);
prepare_index([], _DbId, _KeyFun, Acc) ->
  Acc.

find_by_key(DbId, Path, Fun, AccIn, Options, #{ db := Db} ) ->
  {Key, Offset, Sel} = case Path of
    <<>> -> {<<"$">>, 0, []};
    <<"/">> -> {<<"$">>, 0, []};
    _ ->
      find_key(<< "$.", Path/binary >>)
  end,
  StartKey = case proplists:get_value(start_at, Options) of
               undefined -> nil;
               Start -> << "/", (barrel_lib:to_binary(Start))/binary >>
             end,
  EndKey = case proplists:get_value(end_at, Options) of
             undefined -> nil;
             End -> << "/", (barrel_lib:to_binary(End))/binary >>
           end,
  Max = proplists:get_value(limit_to_last, Options, 0),
  Prefix = idx_forward_path_key(DbId, Key),

  {ok, Snapshot} = erocksdb:snapshot(Db),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOptions = [{gte, StartKey}, {lte, EndKey}, {max, Max}, {read_options, ReadOptions}],

  WrapperFun =
    fun(KeyBin, BinMap, Acc) ->
      PathSize = byte_size(KeyBin) - 1,
      << _Path:PathSize/binary, KeyOffset:8 >> = KeyBin,
      if
        KeyOffset > Offset -> {stop, Acc};
        true ->
          Map = binary_to_term(BinMap),
          case maps:find(Sel, Map) of
            {ok, Entries} ->
              fold_entries(Entries, Fun, Db, DbId, ReadOptions, Acc);
            error ->
              {ok, Acc}
          end
      end
    end,

  try fold_prefix(Db, Prefix, WrapperFun, AccIn, FoldOptions)
  after erocksdb:release_snapshot(Snapshot)
  end.

fold_entries([DocId | Rest], Fun, Db, DbId, ReadOptions, Acc) ->
  {ok, Doc} = get_doc1(
    DbId, DocId, Db, <<>>, false, 0, [], ReadOptions
  ),
  
  case Fun(DocId, Doc, Acc) of
    {ok, Acc2} ->
      fold_entries(Rest, Fun, Db, DbId, ReadOptions, Acc2);
    Else ->
      Else
  end;
fold_entries([], _Fun, _Db, _DbId, _ReadOptions, Acc) ->
  {ok, Acc}.

find_key(Path) ->
  Parts = binary:split(Path, <<".">>, [global]),
  parse_parts(Parts, 0, 0, []).

parse_parts(Parts, _N, Offset, Levels) when length(Parts) =< 3 ->
  {barrel_lib:binary_join(Parts, <<"/">>), Offset, Levels};
parse_parts([<< $[, _/binary >> = Item | Rest], N, Offset, Levels) ->
  [<<>>, BinInt, <<>>] = binary:split(Item, [<<"[">>,<<"]">>], [global]),
  Idx = binary_to_integer(BinInt),
  N2 = N + 1,
  if
    N2 =:= 3 ->
      parse_parts(Rest, 0, Offset + 1, [Idx | Levels]);
    true ->
      parse_parts(Rest, N2, Offset, [Idx | Levels])
  end.

%% key api

meta_key(DbId, Meta) -> << DbId/binary, 0, 0, (barrel_lib:to_binary(Meta))/binary >>.

doc_key(DbId, DocId) -> << DbId/binary, 0, 0, 50, DocId/binary >>.

seq_key(DbId, Seq) -> << DbId/binary, 0, 0, 100, Seq:32>>.

sys_key(DbId, DocId) -> << DbId/binary, 0, 0, 200, DocId/binary>>.

rev_key(DbId, DocId, Rev) -> << DbId/binary, DocId/binary, 1, Rev/binary >>.


idx_last_doc_key(DbId, DocId) -> << DbId/binary, 0, 0, 400, DocId/binary >>.
idx_forward_path_key(DbId, Path) -> << DbId/binary, 0, 0, 410, 0, Path/binary >>.
idx_reverse_path_key(DbId, Path) -> << DbId/binary, 0, 0, 420, 0, Path/binary >>.
