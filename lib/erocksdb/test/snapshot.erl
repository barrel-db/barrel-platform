-module(snapshot).

-compile([export_all/1]).
-include_lib("eunit/include/eunit.hrl").

get_test() ->
    os:cmd("rm -rf test.db"),
    {ok, Db} = erocksdb:open("test.db", [{create_if_missing, true}], []),
    try
        erocksdb:put(Db, <<"a">>, <<"x">>, []),
        ?assertEqual({ok, <<"x">>}, erocksdb:get(Db, <<"a">>, [])),
        {ok, Snapshot} = erocksdb:snapshot(Db),
        erocksdb:put(Db, <<"a">>, <<"y">>, []),
        ?assertEqual({ok, <<"y">>}, erocksdb:get(Db, <<"a">>, [])),
        ?assertEqual({ok, <<"x">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot}])),
        erocksdb:release_snapshot(Snapshot)
    after
        erocksdb:close(Db)
    end.

multiple_snapshot_test() ->
    os:cmd("rm -rf test.db"),
    {ok, Db} = erocksdb:open("test.db", [{create_if_missing, true}], []),
    try
        erocksdb:put(Db, <<"a">>, <<"1">>, []),
        ?assertEqual({ok, <<"1">>}, erocksdb:get(Db, <<"a">>, [])),

        {ok, Snapshot} = erocksdb:snapshot(Db),
        erocksdb:put(Db, <<"a">>, <<"2">>, []),
        ?assertEqual({ok, <<"2">>}, erocksdb:get(Db, <<"a">>, [])),
        ?assertEqual({ok, <<"1">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot}])),

        {ok, Snapshot2} = erocksdb:snapshot(Db),
        erocksdb:put(Db, <<"a">>, <<"3">>, []),
        ?assertEqual({ok, <<"3">>}, erocksdb:get(Db, <<"a">>, [])),
        ?assertEqual({ok, <<"1">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot}])),
        ?assertEqual({ok, <<"2">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot2}])),

        {ok, Snapshot3} = erocksdb:snapshot(Db),
        erocksdb:put(Db, <<"a">>, <<"4">>, []),
        ?assertEqual({ok, <<"4">>}, erocksdb:get(Db, <<"a">>, [])),
        ?assertEqual({ok, <<"1">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot}])),
        ?assertEqual({ok, <<"2">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot2}])),
        ?assertEqual({ok, <<"3">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot3}])),

        erocksdb:release_snapshot(Snapshot),
        erocksdb:release_snapshot(Snapshot2),
        erocksdb:release_snapshot(Snapshot3)
    after
        erocksdb:close(Db)
    end.


iterator_test() ->
    os:cmd("rm -rf ltest"),  % NOTE
    {ok, Ref} = erocksdb:open("ltest", [{create_if_missing, true}], []),
    try
        erocksdb:put(Ref, <<"a">>, <<"x">>, []),
        erocksdb:put(Ref, <<"b">>, <<"y">>, []),
        {ok, I} = erocksdb:iterator(Ref, []),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"y">>},erocksdb:iterator_move(I, next)),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I, prev)),

        {ok, Snapshot} = erocksdb:snapshot(Ref),

        erocksdb:put(Ref, <<"b">>, <<"z">>, []),

        {ok, I2} = erocksdb:iterator(Ref, []),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I2, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"z">>},erocksdb:iterator_move(I2, next)),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I2, prev)),

        {ok, I3} = erocksdb:iterator(Ref, [{snapshot, Snapshot}]),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I3, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"y">>},erocksdb:iterator_move(I3, next)),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I3, prev)),
        erocksdb:release_snapshot(Snapshot)
    after
      erocksdb:close(Ref)
    end.


release_snapshot_test() ->
    os:cmd("rm -rf ltest"),  % NOTE
    {ok, Ref} = erocksdb:open("ltest", [{create_if_missing, true}], []),

    try
        erocksdb:put(Ref, <<"a">>, <<"x">>, []),
        erocksdb:put(Ref, <<"b">>, <<"y">>, []),

        {ok, Snapshot} = erocksdb:snapshot(Ref),

        erocksdb:put(Ref, <<"b">>, <<"z">>, []),

        {ok, I} = erocksdb:iterator(Ref, [{snapshot, Snapshot}]),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"y">>},erocksdb:iterator_move(I, next)),
        ok = erocksdb:release_snapshot(Snapshot),
        ?assertEqual({ok, <<"a">>, <<"x">>}, erocksdb:iterator_move(I, prev)),

        %% snapshot has been released, it can't be reused
        ?assertError(badarg, erocksdb:iterator(Ref, [{snapshot, Snapshot}]))

    after
        erocksdb:close(Ref)
    end.


close_iterator_test() ->
    os:cmd("rm -rf ltest"),  % NOTE
    {ok, Ref} = erocksdb:open("ltest", [{create_if_missing, true}], []),

    try
        erocksdb:put(Ref, <<"a">>, <<"x">>, []),
        erocksdb:put(Ref, <<"b">>, <<"y">>, []),

        {ok, Snapshot} = erocksdb:snapshot(Ref),

        erocksdb:put(Ref, <<"b">>, <<"z">>, []),

        {ok, I} = erocksdb:iterator(Ref, [{snapshot, Snapshot}]),
        ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I, <<>>)),
        ?assertEqual({ok, <<"b">>, <<"y">>},erocksdb:iterator_move(I, next)),

        erocksdb:iterator_close(I),
        erocksdb:release_snapshot(Snapshot)
    after
        erocksdb:close(Ref)
    end.

db_close_test() ->
    os:cmd("rm -rf ltest"),  % NOTE
    {ok, Ref} = erocksdb:open("ltest", [{create_if_missing, true}], []),

    erocksdb:put(Ref, <<"a">>, <<"x">>, []),
    erocksdb:put(Ref, <<"b">>, <<"y">>, []),

    {ok, Snapshot} = erocksdb:snapshot(Ref),

    erocksdb:put(Ref, <<"b">>, <<"z">>, []),


    {ok, I} = erocksdb:iterator(Ref, [{snapshot, Snapshot}]),
    ?assertEqual({ok, <<"a">>, <<"x">>},erocksdb:iterator_move(I, <<>>)),
    ?assertEqual({ok, <<"b">>, <<"y">>},erocksdb:iterator_move(I, next)),
    ok = erocksdb:release_snapshot(Snapshot),
    ?assertEqual({ok, <<"a">>, <<"x">>}, erocksdb:iterator_move(I, prev)),

    erocksdb:close(Ref),

    %% snapshot has been released when the db was closed, it can't be reused
    ?assertError(badarg, erocksdb:iterator(Ref, [{snapshot, Snapshot}])),


    os:cmd("rm -rf ltest"),
    {ok, Db} = erocksdb:open("ltest", [{create_if_missing, true}], []),
    erocksdb:put(Db, <<"a">>, <<"x">>, []),
    ?assertEqual({ok, <<"x">>}, erocksdb:get(Db, <<"a">>, [])),
    {ok, Snapshot2} = erocksdb:snapshot(Db),
    erocksdb:put(Db, <<"a">>, <<"y">>, []),
    ?assertEqual({ok, <<"x">>}, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot2}])),
    erocksdb:close(Db),

    %% snapshot has been released when the db was closed, it can't be reused
    ?assertError(badarg, erocksdb:get(Db, <<"a">>, [{snapshot, Snapshot2}])).
