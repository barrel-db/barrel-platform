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

%% @doc miscellaneous functions to manipulate JSONs
%%
%% TODO: this part can be optimized in rust or C if needed

-module(barrel_json).

-export([decode_path/1]).
-export([get/2]).
-export([flatten/1]).

decode_path(Path) when is_binary(Path) ->
  decode_path(Path, []);
decode_path(Path) when is_list(Path) ->
  Path;
decode_path(_) ->
  erlang:error(badarg).

decode_path(<<>>, Acc) ->
  lists:reverse(Acc);
decode_path(<< $., Rest/binary >>, Acc) ->
  decode_path(Rest, [<<>> |Acc]);
decode_path(<< $[, Rest/binary >>, Acc) ->
  decode_path(Rest, [<<>> | Acc]);
decode_path(<< $], Rest/binary >>, Acc) ->
  decode_path(Rest, Acc);
decode_path(<<Codepoint/utf8, Rest/binary>>, []) ->
  decode_path(Rest, [<< Codepoint/utf8 >>]);

decode_path(<<Codepoint/utf8, Rest/binary>>, [Current|Done]) ->
  decode_path(Rest, [<< Current/binary, Codepoint/utf8 >> | Done]).

get(Path, Doc) ->
 try get_1(decode_path(Path), Doc, Doc)
 catch error:_ -> erlang:error(badarg)
 end.

get_1([Key | Rest], Obj, _Parent) when is_map(Obj) ->
  get_1(Rest, maps:get(Key, Obj), Obj);
get_1([BinInt | Rest], Obj, _Parent) when is_list(Obj) ->
  Idx = binary_to_integer(BinInt) + 1, %% erlang lists start at 1
  get_1(Rest, lists:nth(Idx, Obj), Obj);
get_1([Key], Obj, Parent) ->
  case jsx:decode(Key) of
    Obj -> Parent;
    _ -> erlang:error(badarg)
  end;
get_1([], Obj, _Parent) ->
  Obj.

%% @doc flatten a json in triplets
flatten(Obj) ->
  Flat = maps:fold(
    fun
      (<<"_", _/binary >>, _, Acc) -> Acc;
      (Key, Val, Acc) when is_map(Val) ->
        json_obj(Val, [Key, <<"$">>],  Acc);
      (Key, Val, Acc) when is_list(Val) ->
        json_array(Val, [Key, <<"$">>], Acc);
      (Key, Val, Acc) ->
        [[Val, Key, <<"$">>] | Acc]
    end,
    [],
    Obj
  ),
  Flat.

json_obj(Obj, Root, Acc0) ->
  maps:fold(
    fun
      (Key, Val, Acc) when is_map(Val) ->
        case maybe_split([Key | Root]) of
          {true, NRoot, Path} ->
            json_obj(Val, NRoot, [Path | Acc]);
          {false, NRoot, _} ->
            json_obj(Val, NRoot, Acc)
        end;
      (Key, Val, Acc) when is_list(Val) ->
        case maybe_split([Key | Root]) of
          {true, NRoot, Path} ->
            json_array(Val, NRoot, [Path | Acc]);
          {false, NRoot, _} ->
            json_array(Val, NRoot, Acc)
        end;
      (Key, Val, Acc) ->
        [ [Val, Key | lists:droplast(Root)], [Key | Root] | Acc ]
    end,
    Acc0,
    Obj
  ).

json_array(Arr, Root, Acc) -> json_array_1(Arr, Root, 0, Acc).

json_array_1([Item | Rest], Root, Idx, Acc) when is_map(Item) ->
  Acc1 = case maybe_split([Idx | Root]) of
           {true, NRoot, Path} ->
             json_obj(Item, NRoot, [Path | Acc]);
           {false, NRoot, _} ->
             json_obj(Item, NRoot, Acc)
         end,
  json_array_1(Rest, Root, Idx + 1, Acc1);
json_array_1([Item | Rest], Root, Idx, Acc) when is_list(Item) ->
  Acc1 = case maybe_split([Idx | Root]) of
           {true, NRoot, Path} ->
             json_array(Item, NRoot, [Path | Acc]);
           {false, NRoot, _} ->
             json_array(Item, NRoot, Acc)
         end,
  json_array_1(Rest, Root, Idx + 1, Acc1);
json_array_1([Item | Rest], Root, Idx,  Acc) ->
  Acc1 = [ [Item, Idx | lists:droplast(Root)], [Idx | Root] | Acc ],
  json_array_1(Rest, Root, Idx + 1, Acc1);
json_array_1([], _Root, _Idx, Acc) ->
  Acc.

maybe_split(Parts) ->
  Len = length(Parts),
  if
    Len =:= 3 ->
      NParts = lists:droplast(Parts),
      {true, NParts, Parts};
    true ->
      {false, Parts, nil}
  end.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(doc,
  #{
    <<"a">> => 1,
    <<"b">> => <<"2">>,
    <<"c">> => #{
        <<"a">> => 1,
        <<"b">> => [<<"a">>, <<"b">>, <<"c">>],
        <<"c">> => #{ <<"a">> => 1}
       },
    <<"d">> => [<<"a">>, <<"b">>, <<"c">>],
    <<"e">> => [#{<<"a">> => 1}, #{ <<"b">> => 2}]
   }).

decode_path_test() ->
  ?assertEqual([<<"c">>, <<"a">>], decode_path(<<"c.a">>)),
  ?assertEqual([<<"c">>, <<"b">>, <<"0">>], decode_path(<<"c.b[0]">>)),
  ?assertEqual([<<"c">>, <<"a">>], [<<"c">>, <<"a">>]),
  ?assertError(badarg, decode_path(1)).


get_test() ->
  ?assertEqual(1, get(<<"a">>, ?doc)),
  ?assertEqual(1, get(<<"c.a">>, ?doc)),
  ?assertEqual(1, get(<<"c.c.a">>, ?doc)),
  ?assertEqual([<<"a">>, <<"b">>, <<"c">>], get(<<"c.b">>, ?doc)),
  ?assertEqual(<<"a">>, get(<<"c.b[0]">>, ?doc)),
  ?assertEqual(<<"b">>, get(<<"c.b[1]">>, ?doc)),
  ?assertEqual(1, get(<<"e[0].a">>, ?doc)),
  ?assertEqual(?doc, get(<<"a.1">>, ?doc)),
  ?assertError(badarg, get(<<"a.c">>, ?doc)).

flatten_test() ->
  Expected = [
    [1, <<"a">>, <<"$">>],
    [<<"2">>, <<"b">>, <<"$">>],
    [<<"a">>, <<"c">>, <<"$">>],
    [1, <<"a">>, <<"c">>],
    [<<"b">>, <<"c">>, <<"$">>],
    [0, <<"b">>, <<"c">>],
    [<<"a">>, 0, <<"b">>],
    [1, <<"b">>, <<"c">>],
    [<<"b">>, 1, <<"b">>],
    [2, <<"b">>, <<"c">>],
    [<<"c">>, 2, <<"b">>],
    [<<"c">>, <<"c">>, <<"$">>],
    [<<"a">>, <<"c">>, <<"c">>],
    [1, <<"a">>, <<"c">>],
    [2, <<"d">>, <<"$">>],
    [<<"c">>, 2, <<"d">>],
    [ 1, <<"d">>, <<"$">>],
    [<<"b">>, 1, <<"d">>],
    [0, <<"d">>, <<"$">>],
    [<<"a">>, 0, <<"d">>],
    [0, <<"e">>, <<"$">>],
    [<<"a">>, 0, <<"e">>],
    [1, <<"a">>, 0],
    [1, <<"e">>, <<"$">>],
    [<<"b">>, 1, <<"e">>],
    [2, <<"b">>, 1]

  ],
  ?assertEqual(lists:sort(Expected), lists:sort(flatten(?doc))).

-endif.
