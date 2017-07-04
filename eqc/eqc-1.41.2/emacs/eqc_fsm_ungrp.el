;; QuickCheck mode definitions for eqc_fsm Ungrouped style

(defvar eqc-menu-eqc_fsm_ungrp
  (eqc-module-menu-prepare
   "Finite State Machine specs - Ungrouped style"
   '(["Complete eqc_fsm spec" tempo-template-eqc-module-eqc_fsm]
     ["--" 'ignore]
     ["Full eqc_fsm module header" tempo-template-eqc-header-eqc_fsm]
     ["--" 'ignore]
     ["Include eqc_fsm" tempo-template-eqc-include-eqc_fsm]
     ["Property for eqc_fsm" tempo-template-eqc-prop-eqc_statem]
     ["--" 'ignore]
     ["new state definition"              tempo-template-eqc-fun-fsm-new_state]
     ["Callback initial_state"            tempo-template-eqc-fun-fsm-initial_state]
     ["Callback initial_state_data"       tempo-template-eqc-fun-fsm-initial_state_data]
     ["Callback next_state_data"          tempo-template-eqc-fun-fsm-next_state_data]
     ["Callback precondition"             tempo-template-eqc-fun-fsm-precond]
     ["Callback postcondition"            tempo-template-eqc-fun-fsm-postcond]
     ["Callback weight"                   tempo-template-eqc-fun-fsm-weight]
     ["Callback priority"                 tempo-template-eqc-fun-fsm-priority]
     ["Callback invariant"                tempo-template-eqc-fun-fsm-invariant]
     ["Callback dynamic_precondition"     tempo-template-eqc-fun-fsm-dynamic_precond]
     ["Callback blocking"                 tempo-template-eqc-fun-fsm-blocking]
     ["Callback call_features"            tempo-template-eqc-fun-fsm-features]
;     ["Callback adapt"                    tempo-template-eqc-fun-fsm-adapt]
     ["Callback precondition_probability" tempo-template-eqc-fun-fsm-precondition_probability]
     ["--" 'ignore]
     ["Symbolic function call" tempo-template-eqc-symbolic-call]
     ["Duplicate function clause" duplicate-function-clause]
     ["Duplicate function clause header" duplicate-function-clause-header]
     ["--" 'ignore]
     ["Clone property to parallel property" change_statem_property_to_parallel]
     ["Parallel property" tempo-template-eqc-prop-eqc_statem-par]
     )
   (eqc-menu-list-expand eqc-fsm-autogen-fun-menu-items))
  )

;; Complete eqc_fsm skeleton
(tempo-define-template "eqc-module-eqc_fsm"
 '((eqc-tempo-include tempo-template-eqc-header-eqc_fsm)
   n

   (ask-record 'rec)

   ;; Prompt here use in several places below
   (p "Name of initial state [init_state]:" init_state t)

   (eqc-tempo-include tempo-template-eqc-fun-fsm-init_state) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-initial_state) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-initial_state_data) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-next_state_data) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-precond) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-fsm-dynamic_precond)) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-postcond) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-fsm-invariant)) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-fsm-blocking)) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-fsm-precondition_probability)) n

   (tempo-save-named 'module "?MODULE")
   (tempo-save-named 'prop (erlang-get-module-from-file-name))
   (eqc-tempo-include tempo-template-eqc-prop-eqc_statem) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-weight)
   )
 "eqc_fsm_ungrouped"
 )

(eqc-fun-pre-def "fsm-new_state"
 '( (p "Name of new state [new_state]:" new_state t) )
 '( "%% @doc Definition of a state. A state is represented by a function," > n
    "%%      listing the transitions from that state, together with generators" > n
    "%%      for the calls to make each transition." > n)
 '( "-spec " (default-insert 'new_state "new_state")
    "(S :: eqc_statem:symbolic_state()) ->" > n
    "        eqc_gen:gen({eqc_fsm:state_name(), eqc_statem:call()})." n)
 '( (default-insert 'new_state "new_state") "(S) ->" > n
    "[ %% {target_state, {call, ?MODULE, target_function, []}}" > n
    "]." > n))

(eqc-fun-def "fsm-init_state"
 '( "%% @doc Definition of the state(s). Each state is represented by a function," > n
    "%%      listing the transitions from that state, together with generators" > n
    "%%      for the calls to make each transition." > n)
 '( "-spec " (default-insert 'init_state "init_state")
    "(S :: eqc_statem:symbolic_state()) ->" > n
    "        eqc_gen:gen({eqc_fsm:state_name(), eqc_statem:call()})." n)
 '( (default-insert 'init_state "init_state") "(S) ->" > n
    "[ %% {target_state, {call, ?MODULE, target_function, []}}" > n
    "]." > n))

(eqc-fun-def "fsm-initial_state"
 '( "%% @doc Returns the <i>state name</i> of the initial state." > n)
 '( "-spec initial_state() -> eqc_fsm:state_name()." > n)
 '( "initial_state() ->" > n
    > (p "Name of initial state [init_state]:" init_state t)
    (default-insert 'init_state "init_state")
    "." n))

(eqc-fun-def "fsm-initial_state_data"
 '( "%% @doc The initial state data." > n)
 '( "-spec initial_eqc_fsm:state_data() -> eqc_fsm:state_data()." > n)
 '( "initial_state_data() ->" > n
    > (if (equal (tempo-lookup-named 'use_rec) "n") "[]" '(l "#" (s rec) "{}"))
    "." n))

(eqc-fun-def "fsm-next_state_data"
 '( "%% @doc Next state transformation for state data." > n
    "%%      Note: S is the current state, From and To are state names." > n)
 '( "-spec next_state_data(From, To, S, Var, Call) -> eqc_fsm:state_data()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name()," n
    "         S    :: eqc_statem:symbolic_state()," n
    "         V    :: eqc_statem:var()," n
    "         Call :: eqc_statem:call()." n)
 '( "next_state_data(_From, _To, S, _Var, {call, _, _, _}) ->" > n
    > "S." p n))

(eqc-fun-def "fsm-precond"
 '( "%% @doc Precondition (for state data). Precondition is checked before" > n
    "%%      command is added to the command sequence" > n)
 '( "-spec precondition(From, To, S, Call) -> boolean()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name(),"  n
    "         S    :: eqc_statem:symbolic_state()," n
    "         Call :: eqc_statem:call()." n)
 '( "precondition(_From, _To, _S, {call, _, _, _}) ->" > n
    > "true." p n))

(eqc-fun-def "fsm-postcond"
 '( "%% @doc Postcondition, checked after command has been evaluated"> n
    "%%      Note: S is the state before next_state_data is evaluated." > n)
 '( "-spec postcondition(From, To, S, Call, Res) -> true | term()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name(),"  n
    "         S    :: eqc_statem:dynamic_state()," n
    "         Call :: eqc_statem:call()," n
    "         Res  :: term()." n)
 '( "postcondition(_From, _To, _S, {call, _, _, _}, _Res) ->" > n
    > "true." p n))

(eqc-fun-def "fsm-invariant"
 '( "%% @doc <i>Optional callback</i>, The invariant is checked for" > n
    "%%      each visited state during test execution." > n)
 '( "-spec invariant(StateName, S) -> boolean()" n
    "    when StateName :: eqc_fsm:state_name()," n
    "         S         :: eqc_statem:dynamic_state()." n)
 '( "invariant(_StateName, _S) ->" > n
    > "true." p n))

(eqc-fun-def "fsm-dynamic_precond"
 '( "%% @doc <i>Optional callback</i>, precondition is checked during" > n
    "%%      execution before a command is call is actually made." > n)
 '( "-spec dynamic_precondition(StateName, S, Call) -> boolean()" n
    "    when StateName :: eqc_fsm:state_name(), " n
    "         S         :: eqc_statem:dynamic_state()," n
    "         Call      :: eqc_statem:call()." n)
  '("dynamic_precondition(_StateName, _S, {call, _, _, _}) ->" > n
    > "true." p n))

(eqc-fun-def "fsm-adapt"
 '( "%% @doc <i>Optional callback</i> - used during shrinking when precondition" > n
    "%%      no longer holds. After adapt, precondition is checked again." n)
 '( "-spec adapt(From, To, S, Call) -> boolean()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name(),"  n
    "         S    :: eqc_statem:symbolic_state()," n
    "         Call :: eqc_statem:call()." n)
 '( "adapt(_From, _To, _S, {call, _, _, _}) ->" > n
    > "true." p n))

(eqc-fun-def "fsm-features"
 '( "%% @doc Collects the features of a symbolic call `Call', executed in" > n
    "%%      dynamic state `S', with result `Res'" > n
    "%%      Features are collected during test execution, not test generation." > n)
 '( "-spec call_features(From, To, S, Call, Res) -> true | term()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name(),"  n
    "         S    :: eqc_statem:dynamic_state()," n
    "         Call :: eqc_statem:call()," n
    "         Res  :: term()." n)
 '( "call_features(_From, _To, _S, {call, _, _, _}, _Res) ->" > n
    > "[]." p n))

(eqc-fun-def "fsm-blocking"
 '( "%% @doc <i>Optional callback</i> Returns true if operation is blocking in current state." > n)
 '( "-spec blocking(StateName, S, Call) -> boolean()" n
    "    when StateName :: eqc_fsm:state_name(), " n
    "         S         :: eqc_statem:symbolic_state()," n
    "         Call      :: eqc_statem:call()." n)
 '( "blocking(_StateName, _S, {call, _, _, _}) ->" > n
    > "false." p n))

(eqc-fun-def "fsm-precondition_probability"
 '( "%% @doc <i>Optional callback</i> Makes analysis more precise when preconditions" > n
    "%%      are present. Should reflect how often a precondition check returns false." n)
 '( "-spec precondition_probability(From, To, Call) -> number()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name(),"  n
    "         Call :: eqc_statem:call()." n)
 '( "precondition_probability(_From, _To, {call, _, _, _}) ->" > n
    > "1.0." p n))

(eqc-fun-def "fsm-weight"
 '( "%% @doc <i> Optional callback</i> Weight for transition(s)." > n
    "%%      Specify how often each transition should be chosen" > n)
 '( "-spec weight(From, To, Call) -> integer()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name(),"  n
    "         Call :: eqc_statem:call()." n)
 '( "weight(_From, _To, {call, _, _, _}) ->" > n
     > "1." p n))

(eqc-fun-def "fsm-priority"
 '( "%% @doc <i> Optional callback</i> Priority for transition(s)." > n
    "%%      Similar to weight, but used in case automatic weights are computed." > n)
 '( "-spec priority(From, To, Call) -> number()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name()," n
    "         Call :: eqc_statem:call()." n)
 '( "priority(_From, _To, {call, _, _, _}) ->" > n
     > "1." p n))


