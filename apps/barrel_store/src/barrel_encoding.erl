-module(barrel_encoding).

-export([encode_uint32_ascending/2, encode_uint32_descending/2,
         decode_uint32_ascending/1, decode_uint32_descending/1,
         encode_uint64_ascending/2, encode_uint64_descending/2,
         decode_uint64_ascending/1, decode_uint64_descending/1,
         encode_varint_ascending/2, encode_varint_descending/2,
         decode_varint_ascending/1, decode_varint_descending/1,
         encode_uvarint_ascending/2, encode_uvarint_descending/2,
         decode_uvarint_ascending/1, decode_uvarint_descending/1
]).

%% @doc  encodes the uint32 value using a big-endian 8 byte representation.
%% The bytes are appended to the supplied buffer and the final buffer is returned.
encode_uint32_ascending(B, V) when is_binary(B), is_integer(V), V >= 0 ->
  << B/binary, V:32/big-integer >>;
encode_uint32_ascending(B, V) when is_binary(B), is_integer(V) ->
  << N:32/native-unsigned >> = << V:32/signed-native >>,
  << B/binary, N:32/big-integer >>;
encode_uint32_ascending(_, _) ->
  erlang:error(badarg).

%% @doc encodes the uint32 value so that it sorts in reverse order, from largest to smallest.
encode_uint32_descending(B, V) when is_integer(V) ->
  encode_uint32_ascending(B, bnot V);
encode_uint32_descending(_, _) ->
  erlang:error(badarg).

%% @doc decodes a uint32 from the input buffer, treating
%% the input as a big-endian 4 byte uint32 representation. The remainder
%% of the input buffer and the decoded uint32 are returned.
decode_uint32_ascending(<< V:32/big-integer, B/binary >>) -> {to_uint32(V), B};
decode_uint32_ascending(_B) -> erlang:error(badarg).

%% @doc decodes a uint32 value which was encoded using `encode_uint32_descending/2'.
decode_uint32_descending(B) ->
  {V, LeftOver} = decode_uint32_ascending(B),
  {to_uint32(bnot V), LeftOver}.


to_uint32(N) ->
  Mask = (1 bsl 32) - 1,
  N band Mask.

%% @doc encodes the uint64 value using a big-endian 8 byte representation.
%% The bytes are appended to the supplied buffer and  the final buffer is returned.
encode_uint64_ascending(B, V) when V >= 0 ->
  << B/binary, V:64/big-integer >>;
encode_uint64_ascending(B, V)  ->
  << N:64/native-unsigned >> = << V:64/signed-native >>,
  << B/binary, N:64/big-integer >>.

%% @doc encodes the uint64 value so that it sorts in  reverse order,
%% from largest to smallest.
encode_uint64_descending(B, V) when is_integer(V) ->
  encode_uint64_ascending(B, bnot V);
encode_uint64_descending(_, _) ->
  erlang:error(badarg).

%% @doc decodes a uint64 from the input buffer, treating
%% the input as a big-endian 8 byte uint64 representation. The remainder
%% of the input buffer and the decoded uint64 are returned.
decode_uint64_ascending(<< V:64/big-integer, B/binary >>) -> {to_uint64(V), B};
decode_uint64_ascending(_B) -> erlang:error(badarg).

%% @doc D decodes a uint64 value which was encoded using `encode_uint_64_descending/2'.
decode_uint64_descending(B) ->
  {V, LeftOver} = decode_uint64_ascending(B),
  {to_uint64(bnot V), LeftOver}.

to_uint64(N) ->
  Mask = (1 bsl 64) - 1,
  N band Mask.

-define(INT_MIN, 16#80).
-define(INT_MAX, 16#fd).
-define(INT_MAX_WIDTH, 8).
-define(INT_ZERO, (?INT_MIN + ?INT_MAX_WIDTH) ).
-define(INT_SMALL, (?INT_MAX - ?INT_ZERO - ?INT_MAX_WIDTH) ).


%% @doc EncodeVarintAscending encodes the int64 value using a variable length
%% (length-prefixed) representation. The length is encoded as a single
%% byte. If the value to be encoded is negative the length is encoded
%% as 8-numBytes. If the value is positive it is encoded as
%% 8+num_bytes. The encoded bytes are appended to the supplied buffer
%% and the final buffer is returned.
encode_varint_ascending(B, V) when V < 0 ->
  encode_varint_ascending_1(B, V);
encode_varint_ascending(B, V) ->
  encode_uvarint_ascending(B, to_uint64(V)).


encode_varint_ascending_1(B, V) when V >= -16#ff ->
  << B/binary, (?INT_MIN + 7), V >>;
encode_varint_ascending_1(B, V) when V >= -16#ffff ->
  << B/binary, (?INT_MIN + 6), (V bsr 8), V >>;
encode_varint_ascending_1(B, V) when V >= -16#ffffff ->
  << B/binary, (?INT_MIN + 5), (V bsr 16), (V bsr 8), V >>;
encode_varint_ascending_1(B, V) when V >= -16#ffffffff ->
  << B/binary, (?INT_MIN + 4), (V bsr 24), (V bsr 16), (V bsr 8), V >>;
encode_varint_ascending_1(B, V) when V >= -16#ffffffffff ->
  << B/binary, (?INT_MIN + 3), (V bsr 32), (V bsr 24),  (V bsr 16), (V bsr 8), V >>;
encode_varint_ascending_1(B, V) when V >= -16#ffffffffffff ->
  << B/binary, (?INT_MIN + 2), (V bsr 40), (V bsr 32), (V bsr 24), (V bsr 16), (V bsr 8), V >>;
encode_varint_ascending_1(B, V) when V >= -16#ffffffffffffff ->
  << B/binary, (?INT_MIN + 1), (V bsr 48), (V bsr 40), (V bsr 32), (V bsr 24),  (V bsr 16), (V bsr 8), V >>;
encode_varint_ascending_1(B, V) ->
  << B/binary, ?INT_MIN, (V bsr 56), (V bsr 48), (V bsr 40), (V bsr 32), (V bsr 24),  (V bsr 16), (V bsr 8), V >>.



%% @doc EncodeVarintDescending encodes the int64 value so that it sorts in reverse
%% order, from largest to smallest.
encode_varint_descending(B, V)  ->
  encode_varint_ascending(B, bnot V).


%% @doc decodes a value encoded by `encode_varint_ascending/2'.
decode_varint_ascending(<<>>) -> erlang:error(badarg);
decode_varint_ascending(<< L, _/binary >> = B) ->
  Length = L - ?INT_ZERO,
  io:format("len is ~p - ~p = ~p~n", [L, ?INT_ZERO, Length]),
  decode_varint_ascending_1(B, Length, -Length).


decode_varint_ascending_1(<< _L, B/binary >>, Len, Len2) when Len < 0, byte_size(B) < Len2 ->
  erlang:error(badarg);
decode_varint_ascending_1(<< _L, B0/binary >>, Len, Len2) when Len < 0 ->
  << B1:Len2/binary, LeftOver/binary >> = B0,
  V = fold_binary(B1,
                  fun(T, V1) ->
                      V2 = (V1 bsl 8)  bor (bnot T) band 16#ff,
                      V2
                  end,
                  0),
  {bnot V, LeftOver};
decode_varint_ascending_1(B, _Len, _Len2) ->
  {V, LeftOver} = decode_uvarint_ascending(B),
  {to_uint64(V), LeftOver}.

%% @doc decodes a value encoded by encode_varint_ascending
decode_varint_descending(B) ->
  {V, LeftOver} = decode_varint_ascending(B),
  {bnot V, LeftOver}.



%% @doc EncodeUvarintAscending encodes the uint64 value using a variable length
%% (length-prefixed) representation. The length is encoded as a single
%% byte indicating the number of encoded bytes (-8) to follow. See
%% `encode_varint_ascending/2' for rationale. The encoded bytes are appended to the
%% supplied buffer and the final buffer is returned.-
-spec encode_uvarint_ascending(B, V) -> B2 when
    B :: binary(),
    V :: integer(),
    B2 :: binary().
encode_uvarint_ascending(B, V) when V =< ?INT_SMALL ->
  << B/binary, (?INT_ZERO + V) >>;
encode_uvarint_ascending(B, V) when V =< 16#ff ->
  << B/binary, (?INT_MAX - 7), V >>;
encode_uvarint_ascending(B, V) when V =< 16#ffff ->
  << B/binary, (?INT_MAX - 6), (V bsr 8), V >>;
encode_uvarint_ascending(B, V) when V =< 16#ffffff ->
  << B/binary, (?INT_MAX - 5), (V bsr 16), (V bsr 8), V >>;
encode_uvarint_ascending(B, V) when V =< 16#ffffffff ->
  << B/binary, (?INT_MAX - 4), (V bsr 24), (V bsr 16), (V bsr 8), V >>;
encode_uvarint_ascending(B, V) when V =< 16#ffffffffff ->
  << B/binary, (?INT_MAX - 3), (V bsr 32), (V bsr 24),  (V bsr 16), (V bsr 8), V >>;
encode_uvarint_ascending(B, V) when V =< 16#ffffffffffff ->
  << B/binary, (?INT_MAX - 2), (V bsr 40), (V bsr 32), (V bsr 24), (V bsr 16), (V bsr 8), V >>;
encode_uvarint_ascending(B, V) when V =< 16#ffffffffffffff ->
  << B/binary, (?INT_MAX - 1), (V bsr 48), (V bsr 40), (V bsr 32), (V bsr 24),  (V bsr 16), (V bsr 8), V >>;
encode_uvarint_ascending(B, V) ->
  << B/binary, ?INT_MAX, (V bsr 56), (V bsr 48), (V bsr 40), (V bsr 32), (V bsr 24),  (V bsr 16), (V bsr 8), V >>.


encode_uvarint_descending(B, 0) ->
  << B/binary, (?INT_MIN + 8) >>;
encode_uvarint_descending(B, V) when V =< 16#ff ->
  V1 = to_uint64(bnot V),
  << B/binary, (?INT_MIN + 7), V1 >>;
encode_uvarint_descending(B, V) when V =< 16#ffff  ->
  V1 = to_uint64(bnot V),
  << B/binary, (?INT_MIN + 6), (V1 bsr 8),  V1 >>;
encode_uvarint_descending(B, V) when V =< 16#ffffff ->
  V1 = to_uint64(bnot V),
  << B/binary, (?INT_MIN + 5), (V1 bsr 16), (V1 bsr 8), V1 >>;
encode_uvarint_descending(B, V) when V =< 16#ffffff ->
  V1 = to_uint64(bnot V),
  << B/binary, (?INT_MIN + 4), (V1 bsr 24), (V1 bsr 16), (V1 bsr 8), V1 >>;
encode_uvarint_descending(B, V) when V =< 16#ffffffff ->
  V1 = to_uint64(bnot V),
  << B/binary, (?INT_MIN + 3), (V1 bsr 32), (V1 bsr 24), (V1 bsr 16), (V1 bsr 8), V1 >>;
encode_uvarint_descending(B, V) when V =< 16#ffffffffff ->
  V1 = to_uint64(bnot V),
  << B/binary, (?INT_MIN + 2), (V1 bsr 40), (V1 bsr 32), (V1 bsr 24), (V1 bsr 16), (V1 bsr 8), V1 >>;
encode_uvarint_descending(B, V) when V =< 16#ffffffffffff ->
  V1 = to_uint64(bnot V),
  << B/binary, (?INT_MIN + 1), (V1 bsr 48), (V1 bsr 40), (V1 bsr 32), (V1 bsr 24), (V1 bsr 16), (V1 bsr 8), V1 >>;
encode_uvarint_descending(B, V) ->
  V1 = bnot V,
  << B/binary, ?INT_MIN, (V1 bsr 56), (V1 bsr 48), (V1 bsr 40), (V1 bsr 32), (V1 bsr 24),
    (V1 bsr 16), (V1 bsr 8), V1 >>.


decode_uvarint_ascending(<<>>) -> erlang:error(badarg);
decode_uvarint_ascending(<< B_0, B/binary >>) ->
  Len = B_0 - ?INT_ZERO,
  decode_uvarint_ascending_1(B, Len, Len - ?INT_SMALL).

decode_uvarint_ascending_1(B, Len, _Len2) when Len =< ?INT_SMALL ->
  {to_uint64(Len), B};
decode_uvarint_ascending_1(_B, _Len, Len2) when Len2 < 0; Len2 > 8 ->
  _ = lager:error("invalid uvarint length of %p", [Len2]),
  erlang:error(badarg);
decode_uvarint_ascending_1(B, _Len, Len2) when byte_size(B) < Len2 ->
  _ = lager:error("insufficient bytes to decode uvarint value ~p", [B]),
  erlang:error(badarg);
decode_uvarint_ascending_1(B0, _Len, Len2) ->
  << B1:Len2/binary, LeftOver/binary >> = B0,
  V = fold_binary(B1,
                  fun(T, V1) ->
                      V2 = (V1 bsl 8) bor to_uint64(T),
                      V2
                  end,
                  0),
  {V, LeftOver}.


decode_uvarint_descending(<<>>) ->
  _ = lager:error("insufficient bytes to decode uvarint value", []),
  erlang:error(badarg);
decode_uvarint_descending(<< B_0, B/binary >>) ->
  Len = ?INT_ZERO - B_0,
  decode_uvarint_descending_1(B, Len).

decode_uvarint_descending_1(_B, Len) when Len < 0; Len > 8 ->
  _ = lager:error("invalid uvarint length of %p", [Len]),
  erlang:error(badarg);
decode_uvarint_descending_1(B, Len) when byte_size(B) < Len ->
  _ = lager:error("insufficient bytes to decode uvarint value ~p", [B]),
  erlang:error(badarg);
decode_uvarint_descending_1(B0, Len) ->
  << B1:Len/binary, LeftOver/binary >> = B0,
  V = fold_binary(B1,
                  fun(T, V1) ->
                      V2 = (V1 bsl 8) bor (to_uint64(bnot T band 16#ff)),
                      V2
                  end,
                 0),
  {V, LeftOver}.

fold_binary(<< C, Rest/binary >>, Fun, Acc) -> fold_binary(Rest, Fun, Fun(C, Acc));
fold_binary(<<>>, _Fun, Acc) -> Acc.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_uint32_ascending_test() ->
  Tests = [{ << 0, 0, 0, 0 >>, 0 },
           { << 0, 0, 0, 1 >>, 1 },
           { << 0, 0, 1, 0 >>, 1 bsl 8 },
           { << 16#ff, 16#ff, 16#ff, 16#ff >>, 1 bsl 32 - 1 }], %% max uint32
  test_encode_decode(Tests, fun encode_uint32_ascending/2, fun decode_uint32_ascending /1).

encode_uint32_descending_test() ->
  Tests = [{ << 16#ff, 16#ff, 16#ff, 16#ff >>, 0 },
           { << 16#ff, 16#ff, 16#ff, 16#fe >>, 1 },
           { << 16#ff, 16#ff, 16#fe, 16#ff >>, 1 bsl 8 },
           { << 0, 0, 0, 0 >>, 1 bsl 32 - 1 }], %% max uint32
  test_encode_decode(Tests, fun encode_uint32_descending/2, fun decode_uint32_descending/1).


encode_uint64_ascending_test() ->
  Tests = [{ << 0, 0, 0, 0, 0, 0, 0, 0 >>, 0 },
           { << 0, 0, 0, 0, 0, 0, 0, 1 >>, 1 },
           { << 0, 0, 0, 0, 0, 0, 1, 0 >>, 1 bsl 8 },
           { << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff >>, 1 bsl 64 - 1 }], %% max uint64
  test_encode_decode(Tests, fun encode_uint64_ascending/2, fun decode_uint64_ascending/1).

encode_uint64_descending_test() ->
  Tests = [{ << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff >>, 0 },
           { << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff , 16#fe >>, 1 },
           { << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#fe, 16#ff >>, 1 bsl 8 },
           { << 0, 0, 0, 0, 0, 0, 0, 0 >>, 1 bsl 64 - 1 }], %% max uint 64
  test_encode_decode(Tests, fun encode_uint64_descending/2, fun decode_uint64_descending/1).


encode_varint_ascending_test() ->
  Tests = [{ << 16#86, 16#ff, 16#00 >>, -1 bsl 8 },
           { << 16#87, 16#ff >>, -1 },
           { << 16#88 >>, 0 },
           { << 16#89 >>, 1 },
           { << 16#f5 >>, 109 },
           { << 16#f6, 16#f70 >>, 112 },
           { << 16#f7, 16#01, 16#00 >>, 1 bsl 8 },
           { << 16#fd, 16#7f, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff >>, 16#7FFFFFFFFFFFFFFF}], %% max int64
  test_encode_decode(Tests, fun encode_varint_ascending/2, fun decode_varint_ascending/1).

encode_varint_descending_test() ->
  Tests = [{ << 16#fd, 16#7f, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff >>, -(16#7FFFFFFFFFFFFFFF + 1) }, %% min int64
           { << 16#fd, 16#7f, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#fe >>, -(16#7FFFFFFFFFFFFFFF + 1) + 1 },
           { << 16#f6, 16#ff >>, -1 bsl 8 },
           { << 16#f5 >>, -110 },
           { << 16#87, 16#ff >>, 0 },
           { << 16#87, 16#fe >>, 1 },
           { << 16#86, 16#fe, 16#ff >>, 1 bsl 8 },
           { << 16#80, 16#80, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00 >>, 16#7FFFFFFFFFFFFFFF }],
  test_encode_decode(Tests, fun encode_varint_descending/2, fun decode_varint_descending/1).

encode_uvarint_ascending_test() ->
  Tests = [{ << 16#88 >>, 0 },
           { << 16#89 >>, 1 },
           { << 16#f5 >>, 109 },
           { << 16#f6, 16#6e >>, 110 },
           { << 16#f7, 16#01, 16#00 >>, 1 bsl 8 },
           { << 16#fd, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff >>, 1 bsl 64 - 1 }],
  test_encode_decode(Tests, fun encode_uvarint_ascending/2, fun decode_uvarint_ascending/1).

encode_uvarint_descending_test() ->
  Tests = [{ << 16#88 >>, 0 },
           { << 16#87, 16#fe >>, 1 },
           { << 16#86, 16#fe, 16#ff >>, 1 bsl 8 },
           { << 16#80, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#01 >>, (1 bsl 64 - 1) - 1 },
           { << 16#80, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00 >>, 1 bsl 64 - 1 }],
  test_encode_decode(Tests, fun encode_uvarint_descending/2, fun decode_uvarint_descending/1).



%% == helpers

test_encode_decode([{Encoded, Value} | Rest], Enc, Dec) ->
  io:format("test ~p vs ~p~n", [Encoded, Value]),
  Encoded = Enc(<<>>, Value),
  io:format("tested ~p vs ~p~n", [Encoded, Value]),
  {Value, <<>>} = Dec(Enc(<<>>, Value)),
  test_encode_decode(Rest, Enc, Dec);
test_encode_decode([], _Enc, _Dec) ->
  ok.


-endif.
