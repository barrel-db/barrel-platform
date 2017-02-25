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

%% @doc helpers to handle JSON for the indexer


-module(barrel_index_json).
-author("benoitc").

%% API
-export([flatten/1]).


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
