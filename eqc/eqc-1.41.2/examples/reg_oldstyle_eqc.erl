-file("eqc-1.41.2/examples/reg_oldstyle_eqc.erl", 0).
-module(reg_oldstyle_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

% Model of states

-define(names,[a,b,c,d]).

-record(state,{pids,	% list of spawned pids
	       regs}).	% list of registered names and pids

initial_state() ->
    #state{pids=[], regs=[]}.

% State transitions

next_state(S,V,{call,?MODULE,spawn,[]}) ->
    S#state{pids=[V | S#state.pids]};
next_state(S,_V,{call,erlang,register,[Name,Pid]}) ->
    S#state{regs=[{Name,Pid} | S#state.regs]};
next_state(S,_V,{call,erlang,unregister,[Name]}) ->
    S#state{regs = lists:keydelete(Name,1,S#state.regs)};
next_state(S,_V,{call,erlang,whereis,[_Name]}) ->
    S.

% Command generator

command(S) ->
    oneof([{call,erlang,register, [name(),elements(S#state.pids)]} ||
	      S#state.pids/=[]] ++
	  [{call,erlang,unregister,[name()]},
	   {call,?MODULE,spawn,[]},
	   {call,erlang,whereis,[name()]}]
	 ).

name() ->
    elements(?names).

% Preconditions

precondition(_S,{call,_,_,_}) ->
    true.

% Postconditions

postcondition(_S,{call,_,_,_},_R) ->
    true.

% Correctness property

prop_registration() ->
    ?FORALL(Cmds,commands(?MODULE),
	    begin
		{_H,_S,Res} = run_commands(?MODULE,Cmds),
                [catch unregister(N) || N<-?names],
		Res==ok
	    end).

spawn() -> 
    spawn(fun() -> loop() end).
loop() -> 
    receive _ -> loop() end.


