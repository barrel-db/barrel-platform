-module(user_default).
-compile(export_all).
-define(DB, <<"testdb3">>).

setup() ->
    sync:go(),

    ok.

init_db() ->
    barrel_httpc_eqc:init_db().

run(X) ->
     barrel_httpc_eqc:run(X).

eqc() ->
%     [] = eqc:module({numtests,250}, barrel_httpc_eqc),
%     [] = eqc:module({numtests,250}, barrel_rpc_eqc),
    [] = eqc:module({numtests,15}, barrel_rpc_events_eqc),
    ok.


postget(Id) ->

    {ok, _,_} = barrel:post(?DB,
                            #{<<99, 111, 110, 116, 101, 110, 116>>
                                  => <<0, 0, 0, 0, 0, 0, 0, 0>>,
                              <<"id">>
                                  => Id
                             },
                            #{}),
    barrel:get(?DB,
               Id,
               #{}).
