-file("eqc-1.41.2/examples/sets_eqc.erl", 0).
%%% File    : sets_eqc.erl
%%% Author  : Thomas Arts <thomas.arts@quviq.com>
%%% Description : QuickCheck properties for sets.erl
%%%               Based on "Testing Data Structures with QuickCheck"
%%% Created : 24 Mar 2010 by Thomas Arts

-module(sets_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% Create a generator for the opaque type "set"

set() ->
  ?SIZED(Size,well_defined(set(Size))).

set(0) ->
  oneof([
         {call,sets,new_set,[]},
         {call,sets,from_list,[list(int())]}]);
set(N) ->
  frequency(
    [{5,set(0)},
     {3,
      ?LAZY(?LETSHRINK([Set],[set(N-1)],
                       {call,sets,add_element,[int(), Set]}))},
     {1,
      ?LAZY(?LETSHRINK([Set],[set(N-1)],
                       {call,sets,del_element,[int(), Set]}))},
     {1,
      ?LAZY(?LETSHRINK([Set1,Set2],[set(N div 2),set(N div 2)],
                       {call,sets,union,[Set1, Set2]}))},
     {1,
      ?LAZY(?LETSHRINK(Sets,list(set(N div 3)),
                       {call,sets,union,[Sets]}))},
     {1,
      ?LAZY(?LETSHRINK([Set1,Set2],[set(N div 2),set(N div 2)],
                       {call,sets,intersection,[Set1, Set2]}))},
     {1,
      ?LAZY(?LETSHRINK(Sets,?LET(L,nat(),vector(L+1,set(N div (L+1)))),
                       {call,sets,intersection,[Sets]}))},
     {1,
      ?LAZY(?LETSHRINK([Set1,Set2],[set(N div 2),set(N div 2)],
                       {call,sets,subtract,[Set1, Set2]}))},
     {1,
      ?LAZY(?LETSHRINK([Set],[set(N div 2)],
                       {call,sets,filter,[function1(bool()), Set]}))}
    ]).


%% Define a model interpretation, i.e. a simplified, obviously correct implementation of the data type

%% Should we use the potentially mutated to_list or not?!
model(S) ->
  lists:sort(sets:to_list(S)).

%% Define set operations on the model

madd_element(E,S) ->
  lists:usort([E|S]).

mdel_element(E,S) ->
  S -- [E].

msize(S) ->
  length(S).

mis_element(E,S) ->
  lists:member(E,S).

munion(Ss) ->
  lists:usort(lists:append(Ss)).

mintersection(Sets) ->
  [ E || E<-lists:usort(lists:append(Sets)),
         lists:all(fun(Set) -> lists:member(E,Set) end, Sets)].

mis_disjoint(S1,S2) ->
  mintersection([S1,S2]) == [].

mfilter(Pred,S) ->
  [ E || E<-S, Pred(E)].


%% Define one property for each operation
%% Since all properties have a generator as argument, use eqc:module/2 for checking all
%% of them in one go.

prop_create() ->
  ?FORALL(Set,set(),
          sets:is_set(eval(Set))).

prop_add_element() ->
  ?FORALL({E,Set},{nat(),set()},
          model(sets:add_element(E,eval(Set))) == madd_element(E,model(eval(Set)))
          ).

prop_del_element() ->
  ?FORALL({E,Set},{nat(),set()},
          model(sets:del_element(E,eval(Set))) == mdel_element(E,model(eval(Set)))).

prop_union() ->
  ?FORALL(Sets,list(set()),
          model(sets:union(eval(Sets))) == munion([model(Set) || Set<-eval(Sets)])).

prop_union2() ->
  ?FORALL({Set1,Set2},{set(),set()},
          model(sets:union(eval(Set1),eval(Set2))) == munion([model(eval(Set1)),model(eval(Set2))])).

prop_intersection2() ->
  ?FORALL({Set1,Set2},{set(),set()},
          model(sets:intersection(eval(Set1),eval(Set2))) ==
          mintersection([model(eval(Set1)),model(eval(Set2))])).


prop_intersection() ->
  ?FORALL(Sets,eqc_gen:non_empty(list(set())),
          model(sets:intersection(eval(Sets))) == mintersection([model(Set) || Set<-eval(Sets)])).

prop_size() ->
  ?FORALL(Set,set(),
          sets:size(eval(Set)) == msize(model(eval(Set)))).

prop_is_element() ->
  ?FORALL({E,Set},{nat(),set()},
          sets:is_element(E,eval(Set)) == mis_element(E,model(eval(Set)))).

prop_is_disjoint() ->
  ?FORALL({Set1,Set2},{set(),set()},
          sets:is_disjoint(eval(Set1),eval(Set2)) == mis_disjoint(model(eval(Set1)),model(eval(Set2)))).

prop_filter() ->
  ?FORALL({Pred,Set},{function1(bool()),set()},
          model(sets:filter(Pred,eval(Set))) == mfilter(Pred,model(eval(Set)))).


