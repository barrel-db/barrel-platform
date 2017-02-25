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

-module(barrel_query).

-export([query/6]).

-include("barrel_store.hrl").


query(Db = #db{id=DbId}, Path0, Fun, Acc, order_by_key, FilterOpts) ->
  Path1 = valid_path(Path0),
  Parts = partial_path(barrel_json:decode_path(Path1)),
  StartKey = case proplists:get_value(start_at, FilterOpts) of
               undefined -> nil;
               Start when is_binary(Start) ->
                 StartParts = Parts ++ [Start],
                 barrel_keys:idx_forward_path_key(StartParts)
             end,
  EndKey = case proplists:get_value(end_at, FilterOpts) of
             undefined -> nil;
             End when is_binary(End) ->
               EndParts = Parts ++ [End],
               barrel_keys:idx_forward_path_key(EndParts)

           end,
  Prefix = barrel_keys:idx_forward_path_key(Parts),
  query1(Db, Prefix, StartKey, EndKey, Fun, Acc, Path0, FilterOpts);
query(Db = #db{id=DbId}, Path0, Fun, Acc, order_by_value, FilterOpts) ->
  Path1 = valid_path(Path0),
  Parts = reverse_partial_path(barrel_json:decode_path(Path1)),
  StartKey = case proplists:get_value(start_at, FilterOpts) of
               undefined -> nil;
               Start when is_binary(Start) ->
                 StartParts = Parts ++ [Start],
                 barrel_keys:idx_reverse_path_key(StartParts)
             end,
  EndKey = case proplists:get_value(end_at, FilterOpts) of
             undefined -> nil;
             End when is_binary(End) ->
               EndParts = Parts ++ [End],
               barrel_keys:idx_reverse_path_key(EndParts)
  
           end,
  Prefix = barrel_keys:idx_reverse_path_key(Parts),
  query1(Db, Prefix, StartKey, EndKey, Fun, Acc, Path0, FilterOpts);
query(_, _, _, _, _, _) ->
  erlang:error(badarg).

query1(#db{store=Store}=Db, Prefix, StartKey, EndKey, Fun, AccIn, Path, Opts) ->
  Max = proplists:get_value(limit_to_last, Opts, 0),
  WithMeta = proplists:get_value(meta, Opts, 0),
  {ok, Snapshot} = rocksdb:snapshot(Store),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOptions =
    [{gte, StartKey},
     {lte, EndKey},
     {max, Max},
     {read_options, ReadOptions}],
  
  WrapperFun =
    fun(_KeyBin, BinEntries, Acc) ->
      Entries = binary_to_term(BinEntries),
      fold_entries(Entries, Fun, Path, WithMeta, Db, ReadOptions, Acc)
    end,

  try barrel_rocksdb:fold_prefix(Store, Prefix, WrapperFun, AccIn, FoldOptions)
  after rocksdb:release_snapshot(Snapshot)
  end.

fold_entries([RID | Rest], Fun, Path, WithMeta, Db = #db{store=Store}, ReadOptions, Acc) ->
  Res = rocksdb:get(Store, barrel_keys:res_key(RID), ReadOptions),
  case Res of
    {ok, Bin} ->
      DocInfo = binary_to_term(Bin),
      DocId = maps:get(id, DocInfo),
      {ok, Doc, _Meta} = barrel_db:get_current_revision(DocInfo),
      Val = barrel_json:get(Path, Doc),
      case Fun(DocId, Doc, Val, Acc) of
        {ok, Acc2} ->
          fold_entries(Rest, Fun, Path, WithMeta, Db, ReadOptions, Acc2);
        Else ->
          Else
      end;
    _Else ->
      {ok, Acc}
  end;
fold_entries([], _Fun, _Path, _WithMeta, _Db, _ReadOptions, Acc) ->
  {ok, Acc}.

valid_path(<< $/, _/binary >> = Path) -> << $$, $/, Path/binary >>;
valid_path(Path) when is_binary(Path) -> << $$, $/, Path/binary >>;
valid_path(_) -> erlang:error(badarg).

partial_path(Parts) ->
  Len = length(Parts),
  if
    Len =< 3 -> Parts;
    true -> lists:sublist(Parts, Len - 2, Len)
  end.

reverse_partial_path(Parts) ->
  partial_path(lists:reverse(Parts)).

