
-define(CHECK_COMMANDS(HSR, Cluster, Cmds0, Prop),
  eqc_dynamic_cluster:check_commands(Cluster, Cmds0, fun(HSR) -> Prop end)).

-define(SYMBOLIC(X), {'$eqc_symbolic', X}).

-import(eqc_dynamic_cluster, [dynamic_commands/1, dynamic_commands/2]).

