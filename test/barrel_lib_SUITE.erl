
%% Copyright (c) 2016. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Created by benoitc on 13/06/16.

-module(barrel_lib_SUITE).
-author("Benoit Chesneau").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


all() ->
  [t_b64url].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, Config) ->
  Config.


t_b64url(_Config) ->
  ?assertNotEqual(
    binary:match(base64:encode([255,127,254,252]), [<<"=">>, <<"/">>, <<"+">>]),
    nomatch),
  % this codec produce URL safe output
  ?assertEqual(
    binary:match(barrel_lib:encodeb64url([255,127,254,252]), [<<"=">>, <<"/">>, <<"+">>]),
    nomatch),

  ?assertEqual(barrel_lib:decodeb64url(barrel_lib:encodeb64url(<<"foo">>)), <<"foo">>),
  ?assertEqual(barrel_lib:decodeb64url(barrel_lib:encodeb64url(<<"foo1">>)), <<"foo1">>),
  ?assertEqual(barrel_lib:decodeb64url(barrel_lib:encodeb64url(<<"foo12">>)), <<"foo12">>),
  ?assertEqual(barrel_lib:decodeb64url(barrel_lib:encodeb64url(<<"foo123">>)), <<"foo123">>),

  ?assertEqual(barrel_lib:decodeb64url(barrel_lib:encodeb64url("foo")), <<"foo">>),
  ?assertEqual(barrel_lib:decodeb64url(barrel_lib:encodeb64url(["fo", "o1"])), <<"foo1">>),
  ?assertEqual(barrel_lib:decodeb64url(barrel_lib:encodeb64url([255,127,254,252])), <<255,127,254,252>>).
