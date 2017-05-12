%% Copyright 2016-2017, Benoit Chesneau
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

-module(barrel_jsonpointer).

-export([decode_path/1]).
-export([get/2]).

decode_path(Path) when is_binary(Path) ->
  decode_path(Path, []);
decode_path(Path) when is_list(Path) ->
  Path;
decode_path(_) ->
  erlang:error(badarg).

decode_path(<<>>, Acc) ->
  lists:reverse(Acc);
decode_path(<< $~, $0, Rest/binary >>, [Current |Done]) ->
  decode_path(Rest, [ << Current/binary, $~ >> | Done ]);
decode_path(<< $~, $1, Rest/binary >>, [Current |Done]) ->
  decode_path(Rest, [ << Current/binary, $/ >> | Done ]);
decode_path(<< $/, Rest/binary >>, Acc) ->
  decode_path(Rest, [<<>> |Acc]);
decode_path(<<Codepoint/utf8, Rest/binary>>, []) ->
  decode_path(Rest, [<< Codepoint/utf8 >>]);
decode_path(<<Codepoint/utf8, Rest/binary>>, [Current|Done]) ->
  decode_path(Rest, [<< Current/binary, Codepoint/utf8 >> | Done]).

get(Path, Doc) ->
  get_1(decode_path(Path), Doc).

get_1([Key | Rest], Obj) when is_map(Obj) ->
  get_1(Rest, maps:get(Key, Obj));
get_1([BinInt | Rest], Obj) when is_list(Obj) ->
  Idx = binary_to_integer(BinInt) + 1, %% erlang lists start at 1
  get_1(Rest, lists:nth(Idx, Obj));
get_1([Key], Obj) ->
  ToMatch = case jsx:is_json(Key) of
              true -> jsx:decode(Key);
              false -> Key
            end,
  if
    Obj =:= ToMatch -> null;
    true -> erlang:error(badarg)
  end;
get_1([], Obj) ->
  Obj.


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
  ?assertEqual([<<"c">>, <<"a">>], decode_path(<<"c/a">>)),
  ?assertEqual([<<"c">>, <<"b">>, <<"0">>], decode_path(<<"c/b/0">>)),
  ?assertEqual([<<"c">>, <<"a">>], [<<"c">>, <<"a">>]),
  ?assertError(badarg, decode_path(1)).


get_test() ->
  ?assertEqual(1, get(<<"a">>, ?doc)),
  ?assertEqual(1, get(<<"c/a">>, ?doc)),
  ?assertEqual(1, get(<<"c/c/a">>, ?doc)),
  ?assertEqual([<<"a">>, <<"b">>, <<"c">>], get(<<"c/b">>, ?doc)),
  ?assertEqual(<<"a">>, get(<<"c/b/0">>, ?doc)),
  ?assertEqual(<<"b">>, get(<<"c/b/1">>, ?doc)),
  ?assertEqual(1, get(<<"e/0/a">>, ?doc)),
  ?assertEqual(null, get(<<"a/1">>, ?doc)),
  ?assertError(badarg, get(<<"a/c">>, ?doc)).

-endif.
