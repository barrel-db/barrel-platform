-module(test_util).


-export([wait_for_event/3]).

wait_for_event(_, _, 0) ->
    false;
wait_for_event(Sleeptime, Callback, Count) when Count > 0 andalso is_integer(Sleeptime) ->
    case Callback() of
	ok ->
	    true;
	_ ->
	    timer:sleep(Sleeptime),
	    wait_for_event(Sleeptime, Callback, Count -1 )
    end.
