-file("eqc-1.41.2/examples/lock_eqc.erl", 0).
-module(lock_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").
-compile(export_all).

%% The finite states with the events as transitions.
initial_state() ->
  unlocked.

unlocked() ->
  read_transition() ++
    [{locked, lock}].

locked() ->
  read_transition() ++
    [{unlocked, unlock},
     {locked,   write}].

read_transition() ->
  [{history, read}].


%%---- generators
key() ->
    elements([a,b,c,d]).

value() ->
    int().


%% The state machine model
initial_state_data() ->
    [].

lock_args(_From, _To, _S) -> [].

lock() -> lock:lock().

lock_post(_From, _To, _S, _Args, R) -> R == ok.

unlock_args(_From, _To, _S) -> [].

unlock() -> lock:unlock().
  
unlock_post(_From, _To, _S, _Args, R) -> R == ok.


write_args(_From, _To, _S) ->
  [key(),value()].

write_next(_,_,S,_,[Key,Value]) ->
  [{Key,Value}|proplists:delete(Key,S)].

write(Key, Value) -> lock:write(Key, Value).

write_post(_From, _To, _S, _Args, R) -> R == ok.


read_args(_From, _To, _S) -> [key()].

read(Key) -> lock:read(Key).

read_pre(_From, _To, S, [Key]) ->
  proplists:is_defined(Key,S).

read_post(_From, _To, S, [Key], R) ->
  R == proplists:get_value(Key,S).


prop_locker() ->
    ?FORALL(Cmds,commands(?MODULE),
	    begin
		lock:start(),
		{H,_S,Res} = run_commands(?MODULE,Cmds),
		lock:stop(),
		aggregate(zip(state_names(H),command_names(Cmds)),
			  Res == ok)
	    end).

