-import(eqc_gen,
       [return/1,
        noshrink/1,
        timeout/2,
        resize/2,
        parameter/1, parameter/2, with_parameter/3, with_parameters/2,
        choose/2,
        shuffle/1, sublist/1,
        sample/1, sampleshrink/1,
        oneof/1, frequency/1,
        non_empty/1,
        elements/1, growingelements/1, list/1, list/2, 
        shrink_list/1, shrink_genlist/1, vector/2, map/2,
        function0/1, function1/1, function2/1, function3/1, function4/1,
        bool/0, char/0, int/0, shrink_int/3, nat/0, largeint/0,
        real/0, orderedlist/1,
        utf8/0, utf8/1, binary/0, binary/1, bitstring/0, bitstring/1,
        largebinary/0, largebinary/1, largebinary/2,
        default/2, weighted_default/2,
        seal/1,open/1,peek/1,
        fault/2, fault_rate/3, more_faulty/2, less_faulty/2, no_faults/1,
        prop_shrinks_without_duplicates/1, shrink_without_duplicates/1,
        constrain_shrinking/2,
        is_generator/1]).

-allow_multiple_imports([map/2]).     % because eqc_temporal also imports map/2

-import(eqc_symbolic,
        [eval/1,eval/2,defined/1,well_defined/1,pretty_print/1,pretty_print/2,call_names/1]).

-emacs_mode_tag(statistics).
-import(eqc,[collect/2, collect/3,
             classify/3,
             aggregate/2, aggregate/3,
             measure/3,
             check_distribution/4,
             %distribution/0,
             user_info/3,
             with_title/1, with_title/2, with_tag/1, only_top/1, only_top/2,
             count_values/0
             %print_distribution/1
            ]).

-emacs_mode_tag(prop).
-import(eqc,[conjunction/1, disjunction/1,
             equals/2, less_or_equal/2]).

-emacs_mode_tag(eqc).
-import(eqc,[numtests/2,
             fails/1,
             on_output/2,
             on_test/2,
             in_sequence/1, in_parallel/1,
             quickcheck/1,
             features/2,
             counterexample/0, counterexample/1, counterexample/2,
             current_counterexample/0,
             check/2,check/3,
             recheck/1]).

-compile({parse_transform,eqc_warn}).

-define(DELAY(X),fun()->X end).
-define(FORCE(X),(X)()).
-define(LET(X,E,E2),eqc_gen:bind(E,fun(X)->E2 end)).
-define(SIZED(S,G),eqc_gen:sized(fun(S)->G end)).
-define(SUCHTHAT(X,G,P),eqc_gen:suchthat(G,fun(X)->P end,{?FILE,?LINE})).
-define(SUCHTHAT(Tag,X,G,P),eqc_gen:suchthat(G,fun(X)->P end,{?MODULE,Tag})).
-define(SUCHTHATMAYBE(X,G,P),eqc_gen:suchthatmaybe(G,fun(X)->P end)).
-define(SHRINK(G,Gs),eqc_gen:shrinkwith(G,?DELAY(Gs))).
-define(LETSHRINK(Es,Gs,E), eqc_gen:letshrink(Gs,fun(Es) -> E end)).
-define(LAZY(G),eqc_gen:lazy(?DELAY(G))).
-define(IMPLIES(Pre,Prop),eqc:implies(Pre,??Pre,?DELAY(Prop))).
-define(FORALL(X,Gen,Prop),eqc:forall(Gen,fun(X)->Prop end)).
-define(WHENFAIL(Action,Prop),eqc:whenfail(fun(__EqcResult__) -> put(eqc_result, __EqcResult__), Action end,?LAZY(Prop))).
-define(TRAPEXIT(E),eqc:trapexit(?DELAY(E))).
-define(TIMEOUT(Limit,Prop),eqc:timeout_property(Limit,?LAZY(Prop))).
-define(ALWAYS(N,P),eqc:always(N,?DELAY(P))).
-define(SOMETIMES(N,P),eqc:sometimes(N,?DELAY(P))).
-define(ONCEONLY(P),eqc:onceonly(?DELAY(P))).
-define(SETUP(F, P), {eqc_setup, F, ?LAZY(P)}).

