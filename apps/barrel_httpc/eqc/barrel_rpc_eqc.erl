-module(barrel_rpc_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(DB, <<"testdb1">>).

init_db()->
		{ok, _} = application:ensure_all_started(barrel_rest),
    barrel:create_database(#{ <<"database_id">> => ?DB }),
    fun delete_db/0.

delete_db() ->
    ok = barrel:delete_database(?DB),
    ok.


prop_rpc_crud() ->
    ?SETUP(fun init_db/0,
           ?FORALL({Id,Doc},
                   {non_empty(utf8()),
                    non_empty(map(non_empty(utf8()), non_empty(utf8())))},
                   begin
                       Doc1         = maps:put(<<"id">>, Id, Doc),
											 {ok, Id,_r}  = barrel:post(?DB, Doc1, #{}),

											 {ok, Doc1,_} = barrel:get(?DB, Id, #{}),
											 {ok, _,_}    = barrel:delete(?DB, Id, #{}),
											 true

            end)).
