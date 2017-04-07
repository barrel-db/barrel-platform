-module(helpchange).

-export([ start/0
        , post/1
        ]).

start() ->
  {ok, Conn} = barrel_httpc:connect(<<"http://localhost:7080/dbs/testdb">>),
  spawn_link(fun () -> start_change(Conn) end),
  Conn.

start_change(Conn) ->
  process_flag(trap_exit, true),
  Callback = fun(C) -> io:format("change=~p~n",[C]) end,
  barrel_httpc_changes:start_link(Conn, #{since => 0, mode => sse, changes_cb => Callback}),
  loop().

loop() ->
  receive
    Any ->
      io:format("received ~p~n", [Any]),
      loop()
  end.


post(Conn) ->
  Doc = #{<<"v">> => 42},
  barrel_httpc:post(Conn, Doc, []).
