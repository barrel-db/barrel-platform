%% Macros for combination QuickCheck and Common Test

-define(quickcheck(Prop),
        case eqc:counterexample(Prop) of
          true ->
            true;
          false ->
            exit(gave_up);
          _quickcheck_CounterExample ->
            exit(_quickcheck_CounterExample)
        end).


