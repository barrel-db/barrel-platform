%% Expr may not contain self(), need to do parse transform on it!
%% Therefore, ?PULSE should evaluate to pulse:try_to_run or so and
%% by including pulse.hrl a parse transform is applied to the code.

-define(PULSE(Res,Expr,Prop),
  eqc:forall(
       pulse:seed(),
       fun(Seed) ->
          Res = pulse:run_with_seed(fun() -> Expr end, Seed),
          Prop
       end)).


