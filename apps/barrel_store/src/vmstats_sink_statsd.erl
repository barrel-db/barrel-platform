-module(vmstats_sink_statsd).
-behaviour(vmstats_sink).

-export([collect/3]).

collect(counter, Key, Value) ->
    call({counter, Key, Value});
collect(gauge, Key, Value) ->
    call({gauge, Key, Value});
collect(timing, Key, Value) ->
  call({duration, Key, Value}).


call({Type, Key, Value}) ->
  {ok, Env} = application:get_env(barrel_store, metrics),
  Server = proplists:get_value(statsd_server, Env),
  send(Server, list_to_binary(Key), {Type, Value}).


send({Peer, Port}, Key, {counter, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|c">>,
  udp(Peer, Port, Data);

send({Peer, Port}, Key, {gauge, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|g">>,
  udp(Peer, Port, Data);

send({Peer, Port}, Key, {duration, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|ms">>,
  udp(Peer, Port, Data).

udp(Peer, Port, Data) ->
  Fun = fun() ->
            case gen_udp:open(0) of
              {ok, Socket} ->
                _ = gen_udp:send(Socket, Peer, Port, Data),
                gen_udp:close(Socket);
              Error ->
                lagger:error("can not open udp socket to statsd server: ~p", [Error])
            end
        end,
  _ = spawn(Fun),
  ok.
