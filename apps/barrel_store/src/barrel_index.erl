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


-module(barrel_index).
-author("benoitc").

%% API
-export([
  split_path/1, split_path/3,
  diff/2,
  analyze/1
]).


-define(STRING_PRECISION, 100).


%% TODO: make split value configurable. For default go for 3 since most docs are flat anyway.

split_path(Path) -> split_path(Path, [], []).

split_path([P1, P2, P3 | Rest], Forward0, Reverse0) ->
  Forward1 = [[P1, P2, P3] | Forward0],
  Reverse1 = [[P3, P2, P1] | Reverse0],
  split_path([P2, P3 | Rest], Forward1, Reverse1);
split_path(_, Forward, Reverse) ->
  {Forward, Reverse}.

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
      (<<"_attachments">>, _V, Acc) ->
        Acc;
      (K, V, Acc) when is_map(V) ->
        object(V, [<<"$">>, K], Acc);
      (K, V, Acc) when is_list(V) ->
        array(V, [<<"$">>, K], Acc);
      (K, V, Acc) ->
        [[<<"$">>, K, short(V)] | Acc]
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
        [Root ++ [K, short(V)] | Acc]
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
  Acc1 = [Root ++ [Idx, short(Item)] | Acc0 ],
  array(Rest, Root, Idx +1, Acc1);
array([], _Root, _Idx, Acc) ->
  Acc.

short(<< S:100/binary, _/binary >>) -> S;
short(S) when is_binary(S) -> S;
short(S) -> S.

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

-define(doc2,
  #{
    <<"a">> => 1,
    <<"text">> => <<" Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed fringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc, quis gravida magna mi a libero. Fusce vulputate eleifend sapien. Vestibulum purus quam, scelerisque ut, mollis sed, nonummy id, metus. Nullam accumsan lorem in dui. Cras ultricies mi eu turpis hendrerit fringilla. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; In ac dui quis mi consectetuer lacinia. Nam pretium turpis et arcu. Duis arcu tortor, suscipit eget, imperdiet nec, imperdiet iaculis, ipsum. Sed aliquam ultrices mauris. Integer ante arcu, accumsan a, consectetuer eget, posuere ut, mauris. Praesent adipiscing. Phasellus ullamcorper ipsum rutrum nunc. Nunc nonummy metus. Vestibulum volutpat pretium libero. Cras id dui. Aenean ut eros et nisl sagittis vestibulum. Nullam nulla eros, ultricies sit amet, nonummy id, imperdiet feugiat, pede. Sed lectus. Donec mollis hendrerit risus. Phasellus nec sem in justo pellentesque facilisis. Etiam imperdiet imperdiet orci. Nunc nec neque. Phasellus leo dolor, tempus non, auctor et, hendrerit quis, nisi. Curabitur ligula sapien, tincidunt non, euismod vitae, posuere imperdiet, leo. Maecenas malesuada. Praesent congue erat at massa. Sed cursus turpis vitae tortor. Donec posuere vulputate arcu. Phasellus accumsan cursus velit. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed aliquam, nisi quis porttitor congue, elit erat euismod orci, ac placerat dolor lectus quis orci. Phasellus consectetuer vestibulum elit. Aenean tellus metus, bibendum sed, posuere ac, mattis non, nunc. Vestibulum fringilla pede sit amet augue. In turpis. Pellentesque posuere. Praesent turpis. Aenean posuere, tortor sed cursus feugiat, nunc augue blandit nunc, eu sollicitudin urna dolor sagittis lacus. Donec elit libero, sodales nec, volutpat a, suscipit non, turpis. Nullam sagittis. Suspendisse pulvinar, augue ac venenatis condimentum, sem libero volutpat nibh, nec pellentesque velit pede quis nunc. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Fusce id purus. Ut varius tincidunt libero. Phasellus dolor. Maecenas vestibulum mollis diam. Pellentesque ut neque. Pellentesque habitant morbi tristique senectus et netus et">>
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

analyze_long_text_test() ->
  A = lists:sort(analyze(?doc2)),
  [P1, P2] = A,
  ?assertEqual([<<"$">>, <<"a">>, 1], P1),
  [<<"$">>, <<"text">>, PartialText] = P2,
  ?assertEqual(100, byte_size(PartialText)).


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


split_path_test() ->
  Path = [<<"$">>, <<"c">>, <<"b">>, 0, <<"a">>],

  ExpectedForward = [
    [<<"b">>, 0, <<"a">>],
    [<<"c">>, <<"b">>, 0],
    [<<"$">>, <<"c">>, <<"b">>]
  ],

  ExpectedReverse = [ lists:reverse(P) || P <- ExpectedForward ],
  ?assertEqual({ExpectedForward, ExpectedReverse}, split_path(Path)).

-endif.
