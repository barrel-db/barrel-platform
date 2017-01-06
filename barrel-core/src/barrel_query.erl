-module(barrel_query).

-export([query/5, query/6]).

query(Db, Path, Fun, Acc, FilterOpts) ->
  query(Db, Path, Fun, Acc, order_by_key, FilterOpts).

query(Db, Path0, Fun, Acc, order_by_key, FilterOpts) ->
  Path1 = valid_path(Path0),
  Parts = partial_path(barrel_json:decode_path(Path1)),
  StartKey = case proplists:get_value(start_at, FilterOpts) of
               undefined -> nil;
               Start when is_binary(Start) ->
                 StartParts = Parts ++ [Start],
                 barrel_rocksdb:idx_forward_path_key(StartParts)
             end,
  EndKey = case proplists:get_value(end_at, FilterOpts) of
             undefined -> nil;
             End when is_binary(End) ->
               EndParts = Parts ++ [End],
                barrel_rocksdb:idx_forward_path_key(EndParts)

           end,
  Prefix = barrel_rocksdb:idx_forward_path_key(Parts),
  query1(Db, Prefix, StartKey, EndKey, Fun, Acc, Path0, FilterOpts);
query(Db, Path0, Fun, Acc, order_by_value, FilterOpts) ->
  Path1 = valid_path(Path0),
  Parts = reverse_partial_path(barrel_json:decode_path(Path1)),
  StartKey = case proplists:get_value(start_at, FilterOpts) of
               undefined -> nil;
               Start when is_binary(Start) ->
                 StartParts = Parts ++ [Start],
                 barrel_rocksdb:idx_reverse_path_key(StartParts)
             end,
  EndKey = case proplists:get_value(end_at, FilterOpts) of
             undefined -> nil;
             End when is_binary(End) ->
               EndParts = Parts ++ [End],
               barrel_rocksdb:idx_reverse_path_key(EndParts)
  
           end,
  Prefix = barrel_rocksdb:idx_reverse_path_key(Parts),
  query1(Db, Prefix, StartKey, EndKey, Fun, Acc, Path0, FilterOpts);
query(_, _, _, _, _, _) ->
  erlang:error(badarg).

query1(Db, Prefix, StartKey, EndKey, Fun, AccIn, Path, Opts) ->
  Ref = barrel_rocksdb:get_ref(Db),
  Max = proplists:get_value(limit_to_last, Opts, 0),
  {ok, Snapshot} = rocksdb:snapshot(Ref),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOptions =
    [{gte, StartKey},
     {lte, EndKey},
     {max, Max},
     {read_options, ReadOptions}],

  WrapperFun =
    fun(_KeyBin, BinEntries, Acc) ->
        Entries = binary_to_term(BinEntries),
        fold_entries(Entries, Fun, Path, Ref, ReadOptions, Acc)
    end,

  try barrel_rocksdb:fold_prefix(Ref, Prefix, WrapperFun, AccIn, FoldOptions)
  after rocksdb:release_snapshot(Snapshot)
  end.

fold_entries([DocId | Rest], Fun, Path, Ref, ReadOptions, Acc) ->
  Res = barrel_rocksdb:get_doc1(Ref, DocId, <<>>, false, 0, [], ReadOptions),
  case Res of
    {ok, Doc} ->
      Val = barrel_json:get(Path, Doc),
      case Fun(DocId, Val, Acc) of
        {ok, Acc2} ->
          fold_entries(Rest, Fun, Path, Ref, ReadOptions, Acc2);
        Else ->
          Else
      end;
    _ ->
      {ok, Acc}
  end;
fold_entries([], _Fun, _Path, _Ref, _ReadOptions, Acc) ->
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

