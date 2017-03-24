-module(barrel_encoding).

-export([encode_uint32_ascending/2, encode_uint32_descending/2,
         decode_uint32_ascending/1, decode_uint32_descending/1,
         encode_uint64_ascending/2, encode_uint64_descending/2,
         decode_uint64_ascending/1, decode_uint64_descending/1]).

encode_uint32_ascending(B, V) when is_binary(B), is_integer(V), V >= 0 ->
  << B/binary, V:32/big-integer >>;
encode_uint32_ascending(B, V) when is_binary(B), is_integer(V) ->
  << N:32/native-unsigned >> = << V:32/signed-native >>,
  << B/binary, N:32/big-integer >>;
encode_uint32_ascending(_, _) ->
  erlang:error(badarg).

encode_uint32_descending(B, V) when is_integer(V) ->
  encode_uint32_ascending(B, bnot V);
encode_uint32_descending(_, _) ->
  erlang:error(badarg).

decode_uint32_ascending(<< V:32/big-integer, B/binary >>) ->
  {ok, to_uint32(V), B};
decode_uint32_ascending(_B) ->
  erlang:error(badarg).

decode_uint32_descending(B) ->
  {ok, V, LeftOver} = decode_uint32_ascending(B),
  {ok, to_uint32(bnot V), LeftOver}.


to_uint32(N) ->
  Mask = (1 bsl 32) - 1,
  N band Mask.

encode_uint64_ascending(B, V) when is_binary(B), is_integer(V), V >= 0 ->
  << B/binary, V:64/big-integer >>;
encode_uint64_ascending(B, V) when is_binary(B), is_integer(V) ->
  << N:64/native-unsigned >> = << V:64/signed-native >>,
  << B/binary, N:64/big-integer >>;
encode_uint64_ascending(_, _) ->
  erlang:error(badarg).

encode_uint64_descending(B, V) when is_integer(V) ->
  encode_uint64_ascending(B, bnot V);
encode_uint64_descending(_, _) ->
  erlang:error(badarg).

decode_uint64_ascending(<< V:64/big-integer, B/binary >>) ->
  {ok, to_uint64(V), B};
decode_uint64_ascending(_B) ->
  erlang:error(badarg).

decode_uint64_descending(B) ->
  {ok, V, LeftOver} = decode_uint64_ascending(B),
  {ok, to_uint64(bnot V), LeftOver}.

to_uint64(N) ->
  Mask = (1 bsl 64) - 1,
  N band Mask.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_uint32_ascending_test() ->
  << 0, 0, 0, 0 >> = encode_uint32_ascending(<<>>, 0),
  {ok, 0, <<>>} = decode_uint32_ascending(encode_uint32_ascending(<<>>, 0)),
  << 0, 0, 0, 1 >> =  encode_uint32_ascending(<<>>, 1),
  {ok, 1, <<>>} = decode_uint32_ascending(encode_uint32_ascending(<<>>, 1)),
  << 0, 0, 1, 0 >> =  encode_uint32_ascending(<<>>, 1 bsl 8),
  {ok, 1 bsl 8, <<>>} = decode_uint32_ascending(encode_uint32_ascending(<<>>, 1 bsl 8)),
  MaxUint32 = 1 bsl 32 - 1,
  << 16#ff, 16#ff, 16#ff, 16#ff >> =  encode_uint32_ascending(<<>>, MaxUint32),
  {ok, MaxUint32, <<>>} = decode_uint32_ascending(encode_uint32_ascending(<<>>, MaxUint32)).

encode_uint32_descending_test() ->
  << 16#ff, 16#ff, 16#ff, 16#ff >> = encode_uint32_descending(<<>>, 0),
  {ok, 0, <<>>} = decode_uint32_descending(encode_uint32_descending(<<>>, 0)),
  << 16#ff, 16#ff, 16#ff, 16#fe >> =  encode_uint32_descending(<<>>, 1),
  {ok, 1, <<>>} = decode_uint32_descending(encode_uint32_descending(<<>>, 1)),
  << 16#ff, 16#ff, 16#fe, 16#ff >> =  encode_uint32_descending(<<>>, 1 bsl 8),
  {ok, 1 bsl 8, <<>>} = decode_uint32_descending(encode_uint32_descending(<<>>, 1 bsl 8)),
  MaxUint32 = 1 bsl 32 - 1,
  << 0, 0, 0, 0 >> = encode_uint32_descending(<<>>, MaxUint32),
  {ok, MaxUint32, <<>>} = decode_uint32_descending(encode_uint32_descending(<<>>, MaxUint32)).


encode_uint64_ascending_test() ->
  << 0, 0, 0, 0, 0, 0, 0, 0 >> = encode_uint64_ascending(<<>>, 0),
  {ok, 0, <<>>} = decode_uint64_ascending(encode_uint64_ascending(<<>>, 0)),
  << 0, 0, 0, 0, 0, 0, 0, 1 >> =  encode_uint64_ascending(<<>>, 1),
  {ok, 1, <<>>} = decode_uint64_ascending(encode_uint64_ascending(<<>>, 1)),
  << 0, 0, 0, 0, 0, 0, 1, 0 >> =  encode_uint64_ascending(<<>>, 1 bsl 8),
  {ok, 1 bsl 8, <<>>} = decode_uint64_ascending(encode_uint64_ascending(<<>>, 1 bsl 8)),
  MaxUint64 = 1 bsl 64 - 1,
  << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff >> =  encode_uint64_ascending(<<>>, MaxUint64),
  {ok, MaxUint64, <<>>} = decode_uint64_ascending(encode_uint64_ascending(<<>>, MaxUint64)).

encode_uint64_descending_test() ->
  << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff >> = encode_uint64_descending(<<>>, 0),
  {ok, 0, <<>>} = decode_uint64_descending(encode_uint64_descending(<<>>, 0)),
  << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff , 16#fe >> =  encode_uint64_descending(<<>>, 1),
  {ok, 1, <<>>} = decode_uint64_descending(encode_uint64_descending(<<>>, 1)),
  << 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#fe, 16#ff >> =  encode_uint64_descending(<<>>, 1 bsl 8),
  {ok, 1 bsl 8, <<>>} = decode_uint64_descending(encode_uint64_descending(<<>>, 1 bsl 8)),
  MaxUint64 = 1 bsl 64 - 1,
  << 0, 0, 0, 0, 0, 0, 0, 0 >> = encode_uint64_descending(<<>>, MaxUint64),
  {ok, MaxUint64, <<>>} = decode_uint64_descending(encode_uint64_descending(<<>>, MaxUint64)).

-endif.
