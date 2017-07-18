;; QuickCheck mode definitions for eqc_statem Ungrouped style

(defvar eqc-menu-eqc_statem_ungrp
  (eqc-module-menu-prepare
   "State Machine specs - Ungrouped style"
   '(["Complete eqc_statem spec" tempo-template-eqc-module-eqc_statem-ungrouped]
     ["--" 'ignore]
     ["Full statem module header" tempo-template-eqc-header-eqc_statem]
     ["--" 'ignore]
     ["Include eqc_statem" tempo-template-eqc-include-eqc_statem]
     ["State Machine Property" tempo-template-eqc-prop-eqc_statem]
     ["--" 'ignore]
     ["Callback initial_state" tempo-template-eqc-fun-initial_state]
     ["Callback precondition" tempo-template-eqc-fun-precond]
     ["Callback adapt" tempo-template-eqc-fun-adapt]
     ["Callback command" tempo-template-eqc-fun-command]
     ["Callback postcondition" tempo-template-eqc-fun-postcond]
     ["Callback call_features" tempo-template-eqc-fun-features]
     ["Callback next_state" tempo-template-eqc-fun-next_state]
     ["Callback invariant" tempo-template-eqc-fun-invariant]
     ["Callback dynamic_precondition" tempo-template-eqc-fun-dynamic_precond]
     ["Callback blocking" tempo-template-eqc-fun-blocking]
     ["--" 'ignore]
     ["Symbolic function call" tempo-template-eqc-symbolic-call]
     ["Duplicate function clause" duplicate-function-clause]
     ["Duplicate function clause header" duplicate-function-clause-header]
     ["--" 'ignore]
     ["Clone property to parallel property" change_statem_property_to_parallel]
     ["Parallel property" tempo-template-eqc-prop-eqc_statem-par]
     ["--" 'ignore]
     )
   (eqc-menu-list-expand eqc-statem-autogen-fun-menu-items))
  )

;; Complete eqc_statem skeleton
(tempo-define-template "eqc-module-eqc_statem-ungrouped"
 '(
   (eqc-tempo-include tempo-template-eqc-header-eqc_statem)
   n

   (ask-record 'rec)

   (eqc-tempo-include tempo-template-eqc-fun-initial_state) n
   (eqc-tempo-include tempo-template-eqc-fun-command) n
   (eqc-tempo-include tempo-template-eqc-fun-next_state) n
   (eqc-tempo-include tempo-template-eqc-fun-precond) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-dynamic_precond)) n
   (eqc-tempo-include tempo-template-eqc-fun-postcond) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-invariant)) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-blocking)) n

   (tempo-save-named 'module "?MODULE")
   (tempo-save-named 'prop (erlang-get-module-from-file-name))
   (eqc-tempo-include tempo-template-eqc-prop-eqc_statem)
   )
 "eqc_statem_ungrouped"
 )

;; eqc_statem property
(eqc-prop-def "eqc_statem"
 '( (default-p "Property name (prop_NAME)" (erlang-get-module-from-file-name) 'prop) )
 '( "%% @doc Default generated property" > n)
 '( "-spec prop_" (s prop) "() -> eqc:property()." > n)
 `( (default-p "Module for commands" "?MODULE" 'module)
    "prop_" (s prop) "() ->" > n
    (eqc-indent-string 1) "?FORALL(Cmds, commands(" (s module) ")," n
    (eqc-indent-string 1) "begin" n
    "{H, S, Res} = run_commands(Cmds)," > n
    "check_command_names(Cmds," > n
    (eqc-indent-string 3) "measure(length, commands_length(Cmds)," n
    (eqc-indent-string 4) "pretty_commands(" (s module) ", Cmds, {H, S, Res},"  n
    (eqc-indent-string 4 (length "pretty_commands(")) (if (is-region-active) 'r> "Res == ok")
    ")))" > n
    "end)." > n)
 "statem")

;; Parallel property
(eqc-prop-def "eqc_statem-par"
 '( (default-p "Property name (prop_NAME)" (erlang-get-module-from-file-name) 'prop) )
 '( "%% @doc Default generated property" > n)
 '( "-spec prop_" (s prop) "() -> eqc:property()." > n)
 '( "prop_" (s prop) "() ->" > n
    "  ?FORALL(Repetitions, ?SHRINK(1, [10]), " n
    (default-p "Module for commands" "?MODULE]" 'module)
    "  ?FORALL(Cmds, parallel_commands(" (s module) ")," n
    "  ?ALWAYS(Repetitions, " n
    "  begin" n
    "{Seq, Par, Res} = run_parallel_commands(Cmds)," > n
    "pretty_commands(" (s module) ", Cmds, {Seq, Par, Res}," > n
    (if (is-region-active) 'r> "Res == ok")
    ")" > n
    "end)))." > n)
 "statem-par")

;; Morebugs call
(eqc-fun-def "morebugs2"
  '( "%% @doc Run property repeatedly to find as many different bugs as" > n
     "%% possible. Runs for 10 seconds before giving up finding more bugs." > n)
  '( "-spec bugs() -> [eqc_statem:bug()]." n)
  '( "bugs() -> bugs(10)." n))

(eqc-fun-def "morebugs3"
  '( "%% @doc Run property repeatedly to find as many different bugs as" > n
     "%% possible. Runs for N seconds before giving up finding more bugs." > n)
  '( "-spec bugs(non_neg_integer()) -> [eqc_statem:bug()]." n)
  '( "bugs(N) -> bugs(N, [])." n))

(eqc-fun-def "morebugs4"
  '( "%% @doc Run property repeatedly to find as many different bugs as" > n
     "%% possible. Takes testing time and already found bugs as arguments." > n)
  '( "-spec bugs(non_neg_integer(), [eqc_statem:bug()]) -> [eqc_statem:bug()]." n)
  `( (default-p "Property name (prop_NAME)" (erlang-get-module-from-file-name) 'prop)
     "bugs(Time, Bugs) ->" n
     "more_bugs(eqc:testing_time(Time, prop_" (s prop) "()), 20, Bugs)." > n))

(eqc-fun-def  "initial_state"
 '( "%% @doc Returns the state in which each test case starts. (Unless a different" > n
    "%%      initial state is supplied explicitly to, e.g. commands/2.)" > n)
 '( "-spec initial_state() -> eqc_statem:symbolic_state()." > n)
 '( "initial_state() ->" > n
    > (if (null (tempo-lookup-named 'rec)) "[]" '(l "#" (s rec) "{}"))
    "." n))

(eqc-fun-def "precond"
 '( "%% @doc Precondition, checked before command is added to the command sequence." > n)
 '( "-spec precondition(S, Call) -> boolean()" n
    "    when S    :: eqc_statem:symbolic_state()," n
    "         Call :: eqc_statem:call()." n)
 '( "precondition(_S, {call, _, _, _}) ->" > n
    > "true." p n))

(eqc-fun-def "adapt"
 '( "%% @doc <i>Optional callback</i> - used during shrinking when precondition" > n
    "%%      no longer holds. After adapt, precondition is checked again." n)
 '( "-spec adapt(S, Call) -> boolean()" n
    "    when S    :: eqc_statem:symbolic_state()," n
    "         Call :: eqc_statem:call()." n)
 '( "adapt(_S, {call, _, _, _}) ->" > n
    > "true." p n))

(eqc-fun-def "command"
 '( "%% @doc Command generator, S is the current state" > n)
 '( "-spec command(S :: eqc_statem:symbolic_state()) -> eqc_gen:gen(eqc_statem:call())." > n)
 '( "command(_S) ->" > n
    > "oneof([" p > n > "])." n))

(eqc-fun-def "postcond"
 '( "%% @doc Postcondition, checked after command has been evaluated" > n
    "%%      Note: S is the state before next_state(S, _, Call)" > n)
 '( "-spec postcondition(S, Call, Res) -> boolean()" n
    "    when S    :: eqc_statem:symbolic_state()," n
    "         Call :: eqc_statem:call()," n
    "         Res  :: term()." n)
 '( "postcondition(_S, {call, _, _, _}, _Res) ->" > n
    > "true." p n))

(eqc-fun-def "features"
 '( "%% @doc Collects the features of a symbolic call `Call', executed in" > n
    "%%      dynamic state `S', with result `Res'" > n
    "%%      Features are collected during test execution, not test generation." > n)
 '( "-spec call_features(S, Call, Res) -> list(any())" n
    "    when S    :: eqc_statem:dynamic_state()," n
    "         Call :: eqc_statem:call()," n
    "         Res  :: term()." n)
 '( "call_features(_S, {call, _, _, _}, _Res) ->" > n
    > "[]." p n))

(eqc-fun-def "next_state"
 '( "%% @doc Next state transformation, S is the current state. Returns next state." > n)
 '( "-spec next_state(S, Var, Call) -> NewS" > n
    "    when S    :: eqc_statem:symbolic_state() | eqc_state:dynamic_state()," n
    "         Var  :: eqc_state:var() | term()," n
    "         Call :: eqc_statem:call()," n
    "         NewS :: eqc_state:symbolic_state() | eqc_state:dynamic_state()." n)
 '( "next_state(S, _V, {call, _, _, _}) ->" > n
    > "S." p n))

(eqc-fun-def "dynamic_precond"
 '( "%% @doc <i>Optional callback</i>, used to test a precondition during test execution." > n)
 '( "-spec dynamic_precondition(S, Call) -> boolean()" n
    "    when S    :: eqc_statem:symbolic_state()," n
    "         Call :: eqc_statem:call()." n)
 '( "dynamic_precondition(_S, {call, _, _, _}) ->" > n
    > "true." p n))

(eqc-fun-def "invariant"
 '( "%% @doc <i>Optional callback</i>, Invariant, checked for each visited state" > n
    "%%      during test execution." > n)
 '( "-spec invariant(S :: eqc_statem:dynamic_state()) -> boolean()." > n)
 '( "invariant(_S) ->" > n
    > "true." p n))

(eqc-fun-def "blocking"
 '( "%% @doc Returns true if operation is blocking in current state." > n)
 '( "-spec blocking(S, Call) -> boolean()" n
    "    when S    :: eqc_statem:symbolic_state()," n
    "         Call :: eqc_statem:call()." n)
 '( "blocking(_S, {call, _, _, _}) ->" > n
    > "false." p n))

