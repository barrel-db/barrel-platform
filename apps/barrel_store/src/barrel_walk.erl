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

-module(barrel_walk).

-export([walk/5]).

-include("barrel_store.hrl").


-define(DEFAULT_MAX, 10000).


walk(#db{id=Id, store=Store}, Path, Fun, AccIn, Opts) ->
  _ = lager:info(
        "walk(~p,~p,~p,~p,~p)",
        [Id, Path, Fun, AccIn, Opts]
       ),

  OrderBy = proplists:get_value(order_by, Opts, order_by_key),
  Prefix = make_prefix(normalize_path(Path), OrderBy),
  Prefix1 = case proplists:get_value(equal_to, Opts) of
              undefined -> Prefix;
              Val -> << Prefix/binary, Val/binary >>
            end,


  Max= proplists:get_value(max, Opts, proplists:get_value(limit_to_last, Opts, ?DEFAULT_MAX)),
  {ok, Snapshot} = rocksdb:snapshot(Store),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOptions = [{read_options, ReadOptions}, {max, Max} | Opts],
  WrapperFun = fun(KeyBin, _, Acc) ->
                   Rid = parse_rid(KeyBin, OrderBy),
                   Res = rocksdb:get(Store, barrel_keys:res_key(Rid), ReadOptions),
                   case Res of
                     {ok, Bin} ->
                       DocInfo = binary_to_term(Bin),
                       {ok, Doc, Meta} = barrel_db:get_current_revision(DocInfo),
                       Fun(Doc, Meta, Acc);
                     _Else ->
                       {ok, Acc}
                   end
               end,

  try barrel_rocksdb:fold_prefix(Store, Prefix1, WrapperFun, AccIn, FoldOptions)
  after rocksdb:release_snapshot(Snapshot)
  end.


make_prefix(Path, order_by_key) ->
  Prefix =  barrel_keys:prefix(idx_forward_path),
  barrel_keys:encode_path_forward(Prefix, partial_path(Path));
make_prefix(Path, order_by_value) ->
  Prefix = barrel_keys:prefix(idx_reverse_path),
  barrel_keys:encode_path_reverse(Prefix, partial_path(Path)).

normalize_path(<< $$, _/binary >>=Path) -> Path;
normalize_path(<< $/, _/binary >>=Path) -> << $$, Path/binary >>;
normalize_path(Path) when is_binary(Path)-> << $$, $/, Path/binary >>;
normalize_path(_) -> erlang:error(badarg).

partial_path(Path0) ->
  Path1 = normalize_path(Path0),
  Parts = binary:split(Path1, <<"/">>, [global]),
  partial_path(Parts, length(Parts)).

partial_path(Parts, Len) when Len =< 3 -> Parts;
partial_path(Parts, Len) -> lists:sublist(Parts, Len - 2, Len).

parse_rid(Path, OrderBy) ->
  Prefix = case OrderBy of
             order_by_key -> barrel_keys:prefix(idx_forward_path);
             order_by_value -> barrel_keys:prefix(idx_reverse_path)
           end,
  Sz = byte_size(Prefix),
  << Prefix:Sz/binary, Encoded/binary >> = Path,
  [Rid | _] = decode_partial_path(Encoded, []),
  Rid.

decode_partial_path(<<"">>, Parts) ->
  Parts;
decode_partial_path(B, Parts) ->
  case barrel_encoding:pick_encoding(B) of
    int ->
      {P, R} = barrel_encoding:decode_varint_ascending(B),
      decode_partial_path(R, [P | Parts]);
    float ->
      {P, R} = barrel_encoding:decode_float_ascending(B),
      decode_partial_path(R, [P | Parts]);
    literal ->
      {P, R} = barrel_encoding:decode_literal_ascending(B),
      decode_partial_path(R, [P | Parts]);
    bytes ->
      {P, R} = barrel_encoding:decode_binary_ascending(B),
      decode_partial_path(R, [P | Parts])
  end.
