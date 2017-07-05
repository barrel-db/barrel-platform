
-module(barrel_httpc_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-define(DB_URL,<<"http://localhost:7080/dbs/testdb">>).
set(G) ->
		?LET(S, list(G),
				 sets:from_list(S)).

prop_set_equal() ->
		?FORALL({S1,S2},
						{set(int()),set(int())},
						sets:size(sets:union(S1,S2)) ==
								sets:size(sets:union(S2,S1))).

prop_barrel_create_delete() ->
		{ok, _} = application:ensure_all_started(barrel_rest),
		_ = barrel_httpc:delete_database(?DB_URL),
		_ = barrel_httpc:create_database(?DB_URL),
		{ok, Conn} = barrel_httpc:connect(?DB_URL),
		?FORALL(Key,
						binary(),
						begin
								case barrel_httpc:get(Conn, Key, []) of
										{ok, _Doca, #{ <<"rev">> := _RevIda }} ->
												barrel_httpc:delete(Conn, Key, []);
										_ -> ok
								end,

								Doc = #{ <<"id">> => Key, <<"v">> => 1},
								{ok, Key, RevId} = barrel_httpc:post(Conn, Doc, []),
								{ok, _, #{ <<"rev">> := _RevId }} = barrel_httpc:get(Conn, Key, []),
								_ =  barrel_httpc:delete(Conn, Key, [{rev, RevId}]),
								{error, not_found} == barrel_httpc:get(Conn, Key, [])

						end).
