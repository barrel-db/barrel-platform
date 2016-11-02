%% Copyright (c) 2016 Benoît Chesneau.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(checkpoint).
-compile([export_all/1]).
-include_lib("eunit/include/eunit.hrl").


checkpoint_test() ->
    os:cmd("rm -rf test.db"),
    os:cmd("rm -rf test_backup.db"),

    {ok, Ref} = erocksdb:open("test.db", [{create_if_missing, true}], []),
    try
        erocksdb:put(Ref, <<"a">>, <<"x">>, []),
        ?assertEqual({ok, <<"x">>}, erocksdb:get(Ref, <<"a">>, [])),
        ok = erocksdb:checkpoint(Ref, "test_backup.db"),
        ?assert(filelib:is_dir("test_backup.db")),
        erocksdb:put(Ref, <<"a">>, <<"y">>, []),
        ?assertEqual({ok, <<"y">>}, erocksdb:get(Ref, <<"a">>, []))
    after
        erocksdb:close(Ref)
    end,
    {ok, Ref2} = erocksdb:open("test_backup.db", [], []),
    try
        ?assertEqual({ok, <<"x">>}, erocksdb:get(Ref2, <<"a">>, []))
    after
        erocksdb:close(Ref2)
    end.


iterator_test() ->
    os:cmd("rm -rf test.db"),
    os:cmd("rm -rf test_backup.db"),
    {ok, Ref} = erocksdb:open("test.db", [{create_if_missing, true}], []),
    try
        erocksdb:put(Ref, <<"a">>, <<"x">>, []),
        erocksdb:put(Ref, <<"b">>, <<"y">>, []),
        {ok, I} = erocksdb:iterator(Ref, []),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"y">>},erocksdb:iterator_move(I, next)),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I, prev)),
        ok = erocksdb:checkpoint(Ref, "test_backup.db"),
        erocksdb:put(Ref, <<"b">>, <<"z">>, []),

        {ok, I2} = erocksdb:iterator(Ref, []),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I2, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"z">>},erocksdb:iterator_move(I2, next)),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I2, prev))
    after
        erocksdb:close(Ref)
    end,

    {ok, Ref2} = erocksdb:open("test_backup.db", [], []),
    try

        {ok, I3} = erocksdb:iterator(Ref2, []),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I3, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"y">>},erocksdb:iterator_move(I3, next)),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I3, prev))
    after
      erocksdb:close(Ref2)
    end.
