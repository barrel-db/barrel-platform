-module(barrel_websocket).

-export([broadcast_server/1]).

broadcast_server(Pids) ->
    Pids1 = receive
                {register, Pid, Channel} ->
                    broadcast_register(Pid, Channel, Pids);
                {broadcast, Pid, Message} ->
                    broadcast_sendall(Pid, Message, Pids);
                {'DOWN', MRef, process, Pid, _Reason} ->
                    broadcast_down(Pid, MRef, Pids);
                Msg ->
                    io:format("Unknown message: ~p~n", [Msg]),
                    Pids
            end,
    erlang:hibernate(?MODULE, broadcast_server, [Pids1]).

broadcast_register(Pid, Channel, Pids) ->
    MRef = erlang:monitor(process, Pid),
    broadcast_sendall(
      Pid, "connected", dict:store(Pid, {Channel, MRef}, Pids)).

broadcast_down(Pid, MRef, Pids) ->
    Pids1 = case dict:find(Pid, Pids) of
                {ok, {_, MRef}} ->
                    dict:erase(Pid, Pids);
                _ ->
                    Pids
            end,
    broadcast_sendall(Pid, "disconnected", Pids1).

broadcast_sendall(Pid, Msg, Pids) ->
    dict:fold(
      fun (K, {Reply, MRef}, Acc) ->
              try
                  begin
                      Reply(Msg),
                      dict:store(K, {Reply, MRef}, Acc)
                  end
              catch
                  _:_ ->
                      Acc
              end
      end,
      dict:new(),
      Pids).