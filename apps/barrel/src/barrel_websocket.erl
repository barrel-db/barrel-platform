-module(barrel_websocket).

-export([broadcast/1]).

broadcast(Msg) ->
    Pids = gproc:lookup_values({p, l, {log_event_handler, log}}),
    lists:map(fun({_Pid, ReplyChannel}) -> ReplyChannel(Msg) end, Pids).