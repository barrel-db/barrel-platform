-file("eqc-1.41.2/examples/lock.erl", 0).
-module(lock).

-compile(export_all).

%% The test API, a gen_fsm with lock, unlock, etc as functions.

%% Clients interact with the locker via the following functions, which
%% just send synchronous or asynchronous events to the
lock() ->
    gen_fsm:send_event(locker,lock).

unlock() ->
    gen_fsm:send_event(locker,unlock).

write(Key,Val) ->
    gen_fsm:send_event(locker,{write,Key,Val}).

read(Key) ->
    gen_fsm:sync_send_event(locker,{read,Key}).
%% Finally, we need to be able to start the locker
start() ->
    gen_fsm:start({local,locker},?MODULE,[],[]).
%% and to stop it again
stop() ->
    gen_fsm:sync_send_all_state_event(locker,stop).

%% The implementation of the gen_fsm
init([]) ->
    {ok,unlocked,[]}.

unlocked(lock,S) ->
    {next_state,locked,S}.

unlocked({read,Key},Caller,S) ->
    gen_fsm:reply(Caller,proplists:get_value(Key,S)),
    {next_state,unlocked,S}.

locked({write,Key,Value},S) ->
    {next_state,locked,[{Key,Value}|proplists:delete(Key,S)]};
locked(unlock,S) ->
    {next_state,unlocked,S}.
locked({read,Key},Client,S) ->
    gen_fsm:reply(Client,proplists:get_value(Key,S)),
    {next_state,locked,S}.

%% This is achieved by sending a \verb!stop! event to the locker,
%% which can be handled in any state.
handle_sync_event(stop,_,_,_) ->
    {stop,normal,ok,[]}.

%% No special clean-up is required on termination.

terminate(_,_,_) ->
    ok.

