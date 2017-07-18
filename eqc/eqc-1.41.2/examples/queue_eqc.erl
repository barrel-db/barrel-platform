-file("eqc-1.41.2/examples/queue_eqc.erl", 0).
%%% File    : queue_eqc.erl
%%% Author  :  <>
%%% Description : 
%%% Created : 23 Jun 2010 by  <>

-module(queue_eqc).


-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% Generate a symbolic queue expression. A symbolic queue MAY raise an
%% exception when evaluated.

q() ->
    ?SIZED(Size,q(Size)).

q(0) ->
    %% Base cases: no nested queue expression
    oneof([{call,queue,new,[]},
	   {call,queue,from_list,[list(item())]},
	   not_a_queue]);
q(N) ->
    frequency(
      [{1,q(0)},
       {1,?LETSHRINK(
	     [Q1,Q2],vector(2,shrink_q(well_defined(q(N div 2)))),
	     {call,queue,join,[Q1,Q2]})},
       {N,?LETSHRINK(
	     [Q],[shrink_q(well_defined(q(erlang:max(0,N-2))))],
	     frequency(
	       %% Choose operations that enlarge the queue more often
	       %% than operations that reduce its size.
	       [{2,oneof([{call,queue,in,[item(),Q]},
			  {call,queue,in_r,[item(),Q]},
			  {call,queue,cons,[item(),Q]},
			  {call,queue,snoc,[Q,item()]}])},
		{1,oneof([{call,?MODULE,out,[Q]},
			  {call,?MODULE,out_r,[Q]},
			  {call,queue,reverse,[Q]},
			  {call,?MODULE,split,
			   [elements([1,2]),nat(),Q]},
			  {call,queue,drop,[Q]},
			  {call,queue,drop_r,[Q]},
			  {call,queue,tail,[Q]},
			  {call,queue,init,[Q]},
			  {call,?MODULE,filter,[Q]}])}]))}]).

%% Custom queue shrinking: replace by a new, empty queue, or by a
%% queue with the same elements built directly using from_list.

shrink_q(G) ->
    ?SHRINK(
       ?LET(Q,G,
	    ?SHRINK(Q,[{call,queue,from_list,[queue:to_list(eval(Q))]}])),
       [{call,queue,new,[]}]).

item() ->
    nat().

%% Generate an observation of a queue, as another type.

o() ->
    Q = q(),
    oneof([{call,queue,is_queue,[Q]},
	   {call,queue,is_empty,[Q]},
	   {call,queue,len,[Q]},
	   {call,queue,to_list,[Q]},
	   {call,queue,member,[item(),Q]},
	   {call,queue,get,[Q]},
	   {call,queue,get_r,[Q]},
	   {call,queue,peek,[Q]},
	   {call,queue,peek_r,[Q]},
	   {call,queue,head,[Q]},
	   {call,queue,last,[Q]}
	  ]).

%% Some operations in the queue library return structures containing
%% queues. IN order to use these as part of symbolic queue
%% expressions, we define local versions that extract the queue and
%% return it directly.

%% Note that split returns TWO queues, so we give the local version an
%% additional argument to specify which one to return.
split(I,N,Q) ->
    element(I,queue:split(N,Q)).

out(Q) ->
    element(2,queue:out(Q)).

out_r(Q) ->
    element(2,queue:out_r(Q)).

%% To avoid needing to save function values in test cases, we always
%% filter with the "even" function.
filter(Q) ->
    queue:filter(fun(N)->N rem 2==0 end,Q).			 

%% We use a queue model to predict the values of observations. All the
%% queue library functions (and our local variants) are modelled using
%% a representation of queues as lists---see module queue_model.
model({call,_,F,Args}) ->
    apply(queue_model,F,model(Args));
model([H|T]) ->
    [model(H)|model(T)];
model(X) ->
    X.

%% Test that queue operations return the right result when viewed as a
%% list.
prop_queue() ->
    ?FORALL(Q,q(),
	    eqc:features(
	      [],%queue_features(Q),
	      equals(catching(fun()->queue:to_list(eval(Q)) end),
		     catching(fun()->queue_model:to_list(model(Q)) end)))).

queue_features({call,M,F,Args}) ->
    As = eval(Args),
    [{F,queue_feature(Q)} || Q <- As,
			     queue:is_queue(Q)];
queue_features(not_a_queue) ->
    [not_a_queue].

queue_feature(Q) ->
    erlang:min(2,length(lists:usort(queue:to_list(Q)))).
	

%% Test that all queue observations return the right result... this
%% subsumes the property above since to_list is one of the
%% observations.
prop_observe() ->
    ?FORALL(Obs,o(),
	    eqc:features(observer_features(Obs),
			 equals(catching(fun()->eval(Obs) end),
				catching(fun()->model(Obs) end)))).

%% When we compare the real queue library against the model, we don't
%% want to compare exception values (stack traces and the like), so we
%% abstract all exceptions to the same thing.
catching(F) ->
	try F()
	catch _:_ ->
		exception
	end.


observer_features(Obs={call,_,F,_}) ->
    case catching(fun()->eval(Obs) end) of
	A when is_atom(A) ->
	    [{observe,F,A}];
	_ ->
	    [{observe,F,non_atom}]
    end.

observe_suite(N) ->
    eqc:feature_based_test_suite(
      numtests(
	N,
	prop_observe())).

coverage() ->
    {ok,Lines} = cover:analyse(queue,coverage,line),
    ([L || {L,{_,0}} <- Lines]).

uncovered() ->
    {ok,Lines} = cover:analyse(queue,coverage,line),
    Uncovered = [L || {{queue,L},{0,1}} <- Lines],
    [io:format("Uncovered lines:\n~p\n",[Uncovered]) || Uncovered/=[]],
    io:format("Uncovered: ~p%\n",[100*length(Uncovered)/length(Lines)]).

queue_suite() ->
    eqc:feature_based_test_suite(
	numtests(
	  2000,
	  eqc:line_coverage_features(
	    queue,
	    noshrink(prop_queue())))).

%% Evaluate a symbolic term collecting coverage features.

evaluate({call,M,F,Args}) ->
    {Fs,As} = evaluate(Args),
    case As of
	exception ->
	    {Fs,exception};
	_ ->
	    cover:reset(),
	    Ans = try apply(M,F,As) catch _:_ -> exception end,
	    {ok,Lines} = cover:analyse(queue,coverage,line),
	    Covered = [L || {{_,L},{1,0}} <- Lines],
	    ArgFeatures = [queue_feature(Q) || Q <- As,
					       queue:is_queue(Q)],
	    Features = [{L,F,ArgFeatures}
			|| L <- Covered],
	    {Features++Fs,Ans}
    end;
evaluate([H|T]) ->
    {HFs,HV} = evaluate(H),
    case HV of
	exception ->
	    {HFs,HV};
	_ ->
	    {TFs,TV} = evaluate(T),
	    {HFs++TFs,case TV of
			  exception -> exception;
			  _ -> [HV|TV]
		      end}
    end;
evaluate(X) ->
    {[],X}.

prop_queue2() ->
    ?FORALL(Q,q(),
	    begin
		O = {call,queue,to_list,[Q]},
		{Fs,A} = evaluate(O),
		B = catching(fun() -> model(O) end),
		eqc:features(Fs,equals(A,B))
	    end).

queue2_suite() ->
    eqc:feature_based_test_suite(
	numtests(
	  2000,
	  prop_queue2())).

random_test_suite(N) ->
    eqc:feature_based_test_suite(
      numtests(2000*N,
	       eqc:line_coverage_features(
		 queue,
		 noshrink(?SIZED(Size,resize(Size div N,prop_queue())))))).

