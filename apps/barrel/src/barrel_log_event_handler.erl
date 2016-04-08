-module(barrel_log_event_handler).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).
 
init([Broadcaster, Pid]) ->
    {ok, {Broadcaster, Pid}}.
 
handle_event({log, Log}, {Broadcaster, Pid}) ->
    Broadcaster ! {broadcast, self(), message(Log)},
    {ok, {Broadcaster, Pid}};

handle_event(_, State) ->
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.

message(Log) ->
    Pid = proplists:get_value(pid, lager_msg:metadata(Log)),
    Severity = lager_msg:severity(Log),
    lists:concat(["[",get_string(Pid, false),"] [", get_string(Severity, true), "] ", lager_msg:message(Log)]).

get_string(Term, IsAtom) ->
    Str = lists:flatten(io_lib:format("~p",[Term])),
    case IsAtom of
        false -> string:sub_string(Str, 2, length(Str)-1);
        true -> Str
    end.