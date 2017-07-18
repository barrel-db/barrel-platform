-file("eqc-1.41.2/loadexamples/resman_eqc.erl", 0).
%%% @author Thomas Arts <>
%%% @copyright (C) 2014, Thomas Arts
%%% @doc QuickCheck load testing example for resman.erl
%%%
%%% @end
%%% Created :  7 Aug 2014 by Thomas Arts 

-module(resman_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_loadtest.hrl").

-compile(export_all).

user(Pid,Time) -> 
  spawn(fun() ->
            Pid ! use,
	    timer:sleep(Time),
	    Pid ! free
	end).

users(_,0,_)    -> true;
users(Pid,N,Time) -> 
  user(Pid,Time), 
  users(Pid,N-1,Time).

users(_, []) -> true;
users(Pid, [Time|Times]) ->
  user(Pid,Time),
  users(Pid, Times).
  
	
prop_load() ->
  ?FORALL(N, choose(0,1010),
     ?TRAPEXIT(
	 collect(N,
	     users(resman:start(), N, 100)))).


prop_load2() ->
  ?FORALL(N, choose(0,10000),
     ?FORALL(Users, vector(N,elements([100,200,400])),
       ?TRAPEXIT(
	     users(resman:start(), Users)))).

prop_load_exp2() ->
  ?FORALL(N, exponential(10,100),
     ?FORALL(Users, vector(N,elements([100,200,400])),
       ?TRAPEXIT(
	     users(resman:start(), Users)))).


%% Shrinking

prop_load_exp() ->
  ?FORALL(N, exponential(10,1),
     ?TRAPEXIT(
	users(resman:start(), N, 100))).

