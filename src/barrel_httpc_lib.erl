%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2017 16:27
%%%-------------------------------------------------------------------
-module(barrel_httpc_lib).
-author("benoitc").

%% API
-export([
  to_hex/1,
  make_url/3,
  binary_join/2,
  to_binary/1
]).

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) ->
  << <<(to_digit(H)),(to_digit(L))>> || <<H:4,L:4>> <= Bin >>;
to_hex([H|T]) ->
  [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.

-spec make_url(Conn, Path, Qs) -> Url when
  Conn :: barrel_httpc:conn(),
  Path :: binary() | [binary()],
  Qs :: list(),
  Url :: binary().
make_url(#{db_url := DbUrl}, Path, Qs) ->
  hackney_url:make_url(DbUrl, Path, Qs).

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(_) -> error(badarg).

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  to_binary(Part);
binary_join([Head|Tail], Sep) ->
  lists:foldl(
    fun (Value, Acc) -> <<Acc/binary, Sep/binary, (to_binary(Value))/binary>> end,
    to_binary(Head),
    Tail
  ).


