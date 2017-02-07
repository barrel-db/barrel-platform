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

-module(barrel_rocksdb).
-author("Benoit Chesneau").

%% API

-export([
  fold_prefix/5
]).

-include("barrel.hrl").

fold_prefix(Db, Prefix, Fun, AccIn, Opts) ->
  ReadOptions = proplists:get_value(read_options, Opts, []),

  {ok, Itr} = rocksdb:iterator(Db, ReadOptions),
  try do_fold_prefix(Itr, Prefix, Fun, AccIn, parse_fold_options(Opts))
  after rocksdb:iterator_close(Itr)
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
  case rocksdb:iterator_move(Itr, Start) of
    {ok, Start, _V} when Inclusive /= true ->
      fold_prefix_loop(rocksdb:iterator_move(Itr, next), Itr, Fun, AccIn, 0, Opts2);
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
          fold_prefix_loop(rocksdb:iterator_move(Itr, next),
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

parse_fold_options(Opts) ->
  parse_fold_options(Opts, ?default_fold_options).

parse_fold_options([], Options) ->
  Options;
parse_fold_options([{start_key, Start} | Rest], Options)
  when is_binary(Start) or (Start =:= first) ->
  parse_fold_options(Rest, Options#{gte => Start});
parse_fold_options([{end_key, End} | Rest], Options)
  when is_binary(End) or (End == nil) ->
  parse_fold_options(Rest, Options#{lte => End});
parse_fold_options([{gt, GT} | Rest], Options)
  when is_binary(GT) or (GT =:= first) ->
  parse_fold_options(Rest, Options#{gt => GT});
parse_fold_options([{gte, GT} | Rest], Options)
  when is_binary(GT) or (GT =:= first) ->
  parse_fold_options(Rest, Options#{gte =>  GT});
parse_fold_options([{lt, LT} | Rest], Options)
  when is_binary(LT) or (LT == nil) ->
  parse_fold_options(Rest, Options#{lt => LT});
parse_fold_options([{lte, LT} | Rest], Options)
  when is_binary(LT) or (LT == nil) ->
  parse_fold_options(Rest, Options#{lte => LT});
parse_fold_options([{max, Max} | Rest], Options) ->
  parse_fold_options(Rest, Options#{max => Max});
parse_fold_options([_ | Rest], Options) ->
  parse_fold_options(Rest, Options).
