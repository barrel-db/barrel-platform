-module(couch_dbupdates).

-export([handle_dbupdates/3]).


handle_dbupdates(Fun, Acc, Options) ->
    _ = couch_event:subscribe(db_updated),
    try
        loop(Fun, Acc, Options)
    after
        couch_event:unsubscribe(db_updated)
    end.


loop(Fun, Acc, Options) ->
    [{timeout, Timeout}, {heartbeat, Heartbeat}] = Options,
    receive
        {couch_event, db_updated, Event} ->
            case Fun(Event, Acc) of
                {ok, Acc1} ->
                    loop(Fun, Acc1, Options);
                stop ->
                    Fun(stop, Acc)

            end
    after Timeout ->
        case Heartbeat of
            true ->
                case Fun(heartbeat, Acc) of
                {ok, Acc1} ->
                    loop(Fun, Acc1, Options);
                stop ->
                    Fun(stop, Acc)

                end;
            _ ->
                Fun(stop, Acc)
        end
    end.
