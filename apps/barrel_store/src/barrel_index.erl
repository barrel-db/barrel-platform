%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2017 06:05
%%%-------------------------------------------------------------------

%% TODO: add indexing policy
-module(barrel_index).
-author("benoitc").

%% API
-export([
  split_path/3,
  diff/2,
  analyze/1
]).

-define(STRING_PRECISION, 100).


split_path([P1, P2, P3 | Rest], Forward0, Reverse0) ->
  P3_1 = maybe_resize(P3),
  Forward1 = [[P1, P2, P3_1] | Forward0],
  Reverse1 = [[P3, P2, P1_1] | Reverse0],
  split_path(Rest, Forward1, Reverse1);
split_path([], Forward, Reverse) ->
  {Forward, Reverse}.

%% TODO: check if it's the needed precision we want. let it configurable?
maybe_resize(<< P:?STRING_PRECISION/binary, _/binary >>) -> P;
maybe_resize(P) -> P.

%% %% @doc get the operations maintenance to do between
%% 2 instances of a document
-spec diff(D1, D2) -> {Added, Removed} when
  D1 :: map(), %% new instance of the document
  D2 :: map(), %% old instance of the document
  Added :: list(), %% paths added
  Removed :: list(). %% paths removed
diff(D1, D2) ->
  A1 = analyze(D1),
  A2 = analyze(D2),
  Added = A2 -- A1,
  Removed = A1 -- A2,
  {Added, Removed}.

%% @doc analyze a document and yield paths to update
-spec analyze(D) -> [P] when
  D :: map(), %% document body
  P :: list(binary() | integer() | float() | atom()). %% list of path
analyze(D) ->
  maps:fold(
    fun
      (K, V, Acc) when is_map(V) ->
        object(V, [<<"$">>, K], Acc);
      (K, V, Acc) when is_list(V) ->
        array(V, [<<"$">>, K], Acc);
      (K, V, Acc) ->
        [[<<"$">>, K, V] | Acc]
    end,
    [],
    D
  ).

object(Obj, Root, Acc0) ->
  maps:fold(
    fun
      (K, V, Acc) when is_map(V) ->
        object(V, Root ++ [K], Acc);
      (K, V, Acc) when is_list(V) ->
        array(V, Root ++ [K], Acc);
      (K, V, Acc) ->
        [Root ++ [K, V] | Acc]
    end,
    Acc0,
    Obj
  ).

array(Arr, Root,  Acc) -> array(Arr, Root, 0, Acc).

array([Item | Rest], Root, Idx, Acc0) when is_map(Item) ->
  Acc1 = object(Item, Root ++ [Idx], Acc0),
  array(Rest, Root, Idx + 1, Acc1);
array([Item | Rest], Root, Idx, Acc0) when is_list(Item) ->
  Acc1 = array(Item, Root ++ [Idx], Acc0),
  array(Rest, Root, Idx + 1, Acc1);
array([Item | Rest], Root, Idx, Acc0) ->
  Acc1 = [Root ++ [Idx, Item] | Acc0 ],
  array(Rest, Root, Idx +1, Acc1);
array([], _Root, _Idx, Acc) ->
  Acc.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(doc,
  #{
    <<"a">> => 1,
    <<"b">> => <<"2">>,
    <<"c">> => #{
      <<"a">> => 1,
      <<"b">> => [<<"a">>, <<"b">>, <<"c">>],
      <<"c">> => #{ <<"a">> => 1, <<"b">> => 2}
    },
    <<"d">> => [<<"a">>, <<"b">>, <<"c">>],
    <<"e">> => [#{<<"a">> => 1}, #{ <<"b">> => 2}]
  }).

analyze_test() ->
  Expected = [
    [<<"$">>, <<"a">>, 1],
    [<<"$">>, <<"b">>, <<"2">>],
    [<<"$">>, <<"c">>, <<"a">>, 1],
    [<<"$">>, <<"c">>, <<"b">>, 0, <<"a">>],
    [<<"$">>, <<"c">>, <<"b">>, 1, <<"b">>],
    [<<"$">>, <<"c">>, <<"b">>, 2, <<"c">>],
    [<<"$">>, <<"c">>, <<"c">>, <<"a">>, 1],
    [<<"$">>, <<"c">>, <<"c">>, <<"b">>, 2],
    [<<"$">>, <<"d">>, 0, <<"a">>],
    [<<"$">>, <<"d">>, 1, <<"b">>],
    [<<"$">>, <<"d">>, 2, <<"c">>],
    [<<"$">>, <<"e">>, 0, <<"a">>, 1],
    [<<"$">>, <<"e">>, 1, <<"b">>, 2]
  ],
  ?assertEqual(Expected, lists:sort(analyze(?doc))).


diff_test() ->
  Old = #{ <<"a">> => 1,
           <<"b">> => [0, 1],
           <<"c">> => #{ <<"a">> => 1}},
  New = #{ <<"a">> => 1,
           <<"b">> => [0, 1, 3],
           <<"d">> => #{ <<"a">> => 1}},
  {Added, Removed} = diff(New, Old),
  ?assertEqual([[<<"$">>,<<"c">>,<<"a">>,1]], Added),
  ?assertEqual([[<<"$">>,<<"d">>,<<"a">>,1],[<<"$">>,<<"b">>,2,3]], Removed).

-endif.
