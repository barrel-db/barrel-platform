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
  pre_start/2,
  init/2,
  open_db/2,
  clean_db/3,
  all_dbs/1,
  get_doc_info/3, get_doc_info/4,
  write_doc/6,
  get_doc/7,
  fold_by_id/5,
  changes_since/5,
  last_update_seq/2
]).

-define(VERSION, 1).


pre_start(Name, Options) ->
  {ok, _} = barrel_ext_sup:start_proc({rocksdb_backend, Name}, barrel_rocksdb_backend,
    start_link, [Name, Options], [{restart, permanent}, {shutdown, 5000}]).

init(Name, _Options) ->
  {Backend, _} = gproc:await({n, l, {rocksdb_backend, Name}}, 5000),
  Db = barrel_rocksdb_backend:get_db(Backend),
  {ok, #{db => Db }}.

open_db(#{ db := Db }, Name) ->
  DbKey = <<1, 0, (barrel_lib:to_binary(Name))/binary >>,
  case erocksdb:get(Db, DbKey, []) of
    {ok, DbId} ->
      UpdateSeq = get_update_seq(Db, DbId),
      {DbId, UpdateSeq};
    not_found ->
      DbId = barrel_lib:uniqid(),
      Batch =  [
        {put, DbKey, DbId},
        {put, meta_key(DbId, <<"version">>), integer_to_binary(?VERSION)},
        {put, meta_key(DbId, <<"update_seq">>), integer_to_binary(0)}
      ],
      ok = erocksdb:write(Db,Batch, [{sync, true}]),
      {DbId, 0}
  end.

clean_db(Name, DbId, #{db := Db}) ->
  ok = erocksdb:delete(Db, << 1, 0, Name/binary >>, [{sync, true}]),
  spawn(fun() -> clean_db1(Db, DbId) end),
  ok.

all_dbs(#{db := Db}) ->
  Fun = fun
          (<< 1, 0, Name/binary >>, _, Acc) -> {ok, [Name | Acc]};
          (_, _, _Acc) -> stop
        end,
  AllDbs = fold_prefix(Db, << 1, 0 >>, Fun, []),
  lists:usort(AllDbs).

clean_db1(Db, DbId) ->
  (catch clean_db_prefix(Db, << 10, 0, DbId/binary, 0 >>)),
  (catch clean_db_prefix(Db, << 100, 0, DbId/binary >>)),
  (catch clean_db_prefix(Db, << 200, 0, DbId/binary >>)),
  (catch clean_db_prefix(Db, << 300, 0, DbId/binary >>)),
  ok.

clean_db_prefix(Db, Prefix) ->
  {ok, Itr} = erocksdb:iterator(Db, []),
  try clean_db_prefix1(erocksdb:iterator_move(Itr, Prefix), Itr, Db, [])
  after erocksdb:iterator_close(Itr)
  end.

clean_db_prefix1({error, iterator_closed}, _Itr, _Db, _Acc) -> ok;
clean_db_prefix1({error, invalid_iterator}, _Itr, Db, Acc) ->
  erocksdb:write(Db, lists:reverse(Acc), []);
clean_db_prefix1({ok, K, _V}, Itr, Db, Acc) ->
  Acc2 = [{delete, K} | Acc],
  Sz = length(Acc2),
  if
    Sz >= 500 ->
      _ = erocksdb:write(Db, Acc2, []),
      clean_db_prefix1(erocksdb:iterator_move(Itr, next), Itr, Db, []);
    true ->
      clean_db_prefix1(erocksdb:iterator_move(Itr, next), Itr, Db, Acc2)
  end.

fold_prefix(Db, Prefix, Fun, AccIn) ->
  fold_prefix(Db, Prefix, Fun, AccIn, []).

fold_prefix(Db, Prefix, Fun, AccIn, Opts) ->
  ReadOptions = proplists:get_value(read_options, Opts, []),

  {ok, Itr} = erocksdb:iterator(Db, ReadOptions),
  try do_fold_prefix(Itr, Prefix, Fun, AccIn, barrel_lib:parse_fold_options(Opts))
  after erocksdb:iterator_close(Itr)
  end.

do_fold_prefix(Itr, Prefix, Fun, AccIn, Opts = #{ gt := GT, gte := GTE, move := Move}) ->
  {Start, Inclusive} = case {GT, GTE} of
                         {nil, nil} -> {Prefix, true};
                         {first, _} -> {Prefix, false};
                         {K, _} when is_binary(K) -> {<< Prefix/binary, K/binary >>, false};
                         _ -> error(badarg)
                       end,
  Opts2 = Opts#{prefix => Prefix},
  case erocksdb:iterator_move(Itr, Start) of
    {ok, {Start, _V}} when Inclusive /= true ->
      fold_prefix_loop(erocksdb:iterator_move(Itr, Move), Itr, Fun, AccIn, 0, Opts2);
    Next ->
      fold_prefix_loop(Next, Itr, Fun, AccIn, 0, Opts2)
  end.

fold_prefix_loop({error, iterator_closed}, _Itr, _Fun, Acc, _N, _Opts) ->
  throw({iterator_closed, Acc});
fold_prefix_loop({error, invalid_iterator}, _Itr, _Fun, Acc, _N, _Opts) ->
  Acc;
fold_prefix_loop({ok, K, _V}=KV, Itr, Fun, Acc, N0, Opts = #{ lt := End})
    when End /= nil orelse K < End ->
  fold_prefix_loop1(KV, Itr, Fun, Acc, N0, Opts);
fold_prefix_loop({ok, K, _V}=KV, Itr, Fun, Acc, N, Opts = #{ lte := End})
    when End =:= nil orelse K < End ->
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
  #{max := Max, move :=  Move, prefix := P} = Opts,
  N = N0 + 1,
  case match_prefix(K, P) of
    true ->
      case Fun(K, V, Acc0) of
        {ok, Acc} when (Max =:= 0) orelse (N < Max) ->
          fold_prefix_loop(erocksdb:iterator_move(Itr, Move),
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
  DocKey = << 100, 0, DbId/binary, DocId/binary >>,
  case erocksdb:get(Db, DocKey, ReadOptions) of
    {ok, BinDocInfo} -> {ok, binary_to_term(BinDocInfo)};
    not_found -> {error, not_found}
  end.


get_update_seq(Db, DbId) ->
  {ok, SeqBin} = erocksdb:get(Db, meta_key(DbId, <<"update_seq">>), []),
  binary_to_integer(SeqBin).

write_doc(DbId, DocId, LastSeq, DocInfo, Body, #{ db := Db}) ->
  #{update_seq := Seq} = DocInfo,
  #{<<"_rev">> := Rev} = Body,
  DocInfoBin = term_to_binary(DocInfo),
  Batch = [
    {put, rev_key(DbId, DocId, Rev), term_to_binary(Body)},
    {put, doc_key(DbId, DocId), DocInfoBin},
    {put, seq_key(DbId, Seq), DocInfoBin},
    {put, meta_key(DbId, <<"update_seq">>), barrel_lib:to_binary(Seq)}
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
    Error -> Error
  end.


get_doc_rev(Db, DbId, DocId, RevId, ReadOptions) ->
  case erocksdb:get(Db, rev_key(DbId, DocId, RevId), ReadOptions) of
    {ok, Bin} -> {ok, binary_to_term(Bin)};
    not_found -> {error, not_found};
    Error -> Error
  end.

fold_by_id(DbId, Fun, AccIn, Opts, #{ db := Db}) ->
  Prefix = << 100, 0, DbId/binary >>,
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

changes_since(DbId, Since, Fun, AccIn, #{ db := Db}) ->
  Prefix = << 200, 0, DbId/binary >>,
  {ok, Snapshot} = erocksdb:snapshot(Db),
  ReadOptions = [{snapshot, Snapshot}],
  Opts = [
           {gt, Since},
           {gte, true},
           {read_options, ReadOptions}
         ],
  IncludeDoc = proplists:get_value(include_doc, Opts, false),
  
  WrapperFun = fun(Key, BinDocInfo, Acc) ->
      DocInfo = binary_to_term(BinDocInfo),
      [_, SeqBin] = binary:split(Key, Prefix),
      Seq = binary_to_integer(SeqBin),
      RevId = maps:get(current_rev, DocInfo),
      DocId = maps:get(id, DocInfo),
      
      Doc = case IncludeDoc of
              true ->
                get_doc_rev(Db, DbId, DocId, RevId, ReadOptions);
              false -> {ok, nil}
            end,
      
      Fun(Seq, DocInfo, Doc, Acc)
    end,
  
  try fold_prefix(Db, Prefix, WrapperFun, AccIn, Opts)
  after erocksdb:release_snapshot(Snapshot)
  end.


last_update_seq(DbId, #{db := Db}) -> get_update_seq(Db, DbId).

%% key api

meta_key(DbId, Meta) -> << 10, 0, DbId/binary, 0, (barrel_lib:to_binary(Meta))/binary >>.

doc_key(DbId, DocId) -> << 100, 0, DbId/binary, DocId/binary >>.

seq_key(DbId, Seq) -> << 200, 0, DbId/binary, (barrel_lib:to_binary(Seq))/binary >>.

rev_key(DbId, DocId, Rev) -> << 300, 0, DbId/binary, DocId/binary, "/", Rev/binary >>.
