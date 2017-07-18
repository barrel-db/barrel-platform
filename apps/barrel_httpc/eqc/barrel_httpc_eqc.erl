
-module(barrel_httpc_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-define(DB_URL,<<"http://localhost:7080/dbs/testdb">>).
-include_lib("eunit/include/eunit.hrl").

-define(DB, <<"testdb">>).

init_db()->
		{ok, _} = application:ensure_all_started(barrel_rest),
		barrel:create_database(#{ <<"database_id">> => ?DB }),
    fun delete_db/0.

delete_db() ->
    ok = barrel:delete_database(?DB),
    ok.

prop_barrel_create_delete() ->
		?SETUP(fun init_db/0,
					 ?FORALL(Key,
									 non_empty(utf8()),
									 run(Key)
									 )).


run(Key)->
		{ok, Conn} = barrel_httpc:connect(?DB_URL),
		case barrel_httpc:get(Conn, Key, []) of
				{ok, _Doca, #{ <<"rev">> := _RevIda }} = _E ->
						lager:info("Error ~p", [ _E]),
						barrel_httpc:delete(Conn, Key, [{rev, _RevIda}]);
				{error,not_found} -> ok;
				_E ->
						lager:info("Error ~p", [ _E]),
						ok
		end,

		Doc = #{ <<"id">> => Key, <<"v">> => 1},

		{ok, Key, RevId} = barrel_httpc:post(Conn, Doc, []),
		lager:info("Key ~p revId ~p", [ Key, RevId]),
		{ok, _, #{ <<"rev">> := _RevId }} = barrel_httpc:get(Conn, Key, [{rev, RevId}]),
		_ =  barrel_httpc:delete(Conn, Key, []),
		{error, not_found} == barrel_httpc:get(Conn, Key, []).
