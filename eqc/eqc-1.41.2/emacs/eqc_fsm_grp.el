;; QuickCheck mode definitions for eqc_fsm Grouped style

(defvar eqc-menu-eqc_fsm_grp
  (eqc-module-menu-prepare
   "Finite State Machine specs - Grouped style"
   '(["Complete eqc_fsm spec" tempo-template-eqc-module-eqc_fsm-grouped]
     ["--" 'ignore]
     ["Full eqc_fsm module header" tempo-template-eqc-header-eqc_fsm]
     ["--" 'ignore]
     ["Include eqc_fsm" tempo-template-eqc-include-eqc_fsm]
     ["Property for eqc_fsm" tempo-template-eqc-prop-eqc_statem]
     ["New operation" tempo-template-eqc-fsm-grouped_operation]
     ["New operation - All callbacks" tempo-template-eqc-fsm-grouped_operation-all]
     ["New operation - Interactive" tempo-template-eqc-fsm-grouped_operation-interactive]
     ["--" 'ignore]
     ["Callback command_precondition_common" tempo-template-eqc-fun-fsm-common_prefilter]
     ["Callback precondition_common" tempo-template-eqc-fun-fsm-common_precondition]
     ["Callback postcondition_common" tempo-template-eqc-fun-fsm-common_postcondition]
     ["--" 'ignore]
     ["Callback initial_state" tempo-template-eqc-fun-fsm-initial_state]
     ["Callback initial_state_data" tempo-template-eqc-fun-fsm-initial_state_data]
     ["Callback invariant" tempo-template-eqc-fun-fsm-invariant]
     ["Callback weight" tempo-template-eqc-fun-fsm-weight-grouped]
     ["Callback priority" tempo-template-eqc-fun-fsm-priority-grouped]
     ["Callback precondition_probability" tempo-template-eqc-fun-fsm-precondition_probability]
     ["--" 'ignore]
     ["Operation - add args" tempo-template-eqc-add-args_fsm]
     ["Operation - add pre_3" tempo-template-eqc-add-pre_3]
     ["Operation - add pre_4" tempo-template-eqc-add-pre_4]
     ["Operation - add next" tempo-template-eqc-add-next_fsm]
     ["Operation - add post" tempo-template-eqc-add-post_fsm]
     ["Operation - add blocking" tempo-template-eqc-add-blocking_fsm]
     ["Operation - add features" tempo-template-eqc-add-features_fsm]
;     ["Operation - add adapt" tempo-template-eqc-add-adapt_fsm]
     ["Operation - add dynamic precond." tempo-template-eqc-add-dynpre_fsm]
     ["--" 'ignore]
     ["Clone property to parallel property" change_statem_property_to_parallel]
     ["Parallel property" tempo-template-eqc-prop-eqc_statem-par]
     ["--" 'ignore])
   (eqc-menu-list-expand eqc-fsm-autogen-fun-menu-items))
  )

;; Complete eqc_fsm skeleton
(tempo-define-template "eqc-module-eqc_fsm-grouped"
 '((eqc-tempo-include tempo-template-eqc-header-eqc_fsm) n

   (ask-record 'rec "state")

   ;; Prompt here use in several places below
   (default-p "Name of initial state"  "init_state" 'init_state)

   (mk-comment-h1 "State and state functions") > n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-initial_state) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-initial_state_data) n

   (mk-comment-h1 "Generators") > n n

   (mk-comment-h1 "Common pre-/post-conditions") > n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-common_prefilter) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-common_precondition) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-common_postcondition) n

   (mk-comment-h1 "Transitions") > n
   (tempo-save-named 'new_state (tempo-lookup-named 'init_state))
   (eqc-tempo-include tempo-template-eqc-fun-new_state-grouped) n

   (mk-comment-h1 "Operations") > n n
   (default-p "Name of (first) operation" "op" 'op_name)
   (tempo-save-named 'args "") ;Use default here too many questions
   (tempo-save-named 'params "_Args") ;Use default here too many questions
   (mk-op-comment 'op_name)
   (eqc-tempo-include tempo-template-eqc-fsm-grouped_operation-set)

   "%% --- ... more operations" n n

   (mk-comment-h1 "Property") > n
;   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-invariant)) n
   (tempo-save-named 'module "?MODULE")
   (tempo-save-named 'prop (erlang-get-module-from-file-name))
   (eqc-tempo-include tempo-template-eqc-prop-eqc_statem) n
   (eqc-tempo-include tempo-template-eqc-fun-fsm-weight-grouped)
   )
 "eqc_fsm"
 )

(tempo-define-template "eqc-fsm-grouped_operation-interactive"
 '(& (op_args-p)

     (op_ask "pre/3" 'tempo-template-eqc-fun-grp_pre_3)

     (eqc-tempo-include tempo-template-eqc-fun-grp_args_fsm) n

     (op_ask "pre/4" 'tempo-template-eqc-fun-grp_pre_4)

     (eqc-tempo-include tempo-template-eqc-fun-grp_op) n

     (op_ask "next"     'tempo-template-eqc-fun-grp_next_fsm)
     (op_ask "post"     'tempo-template-eqc-fun-grp_post_fsm)
     (op_ask "blocking" 'tempo-template-eqc-fun-grp_blocking_fsm)
;     (op_ask "adapt"    'tempo-template-eqc-fun-grp_adapt_fsm)
     (op_ask "features" 'tempo-template-eqc-fun-grp_features_fsm)
     (op_ask "dynpre"   'tempo-template-eqc-fun-grp_dynpre_fsm)
  )
 "fiop")

;; Template for a grouped operation.
(tempo-define-template  "eqc-fsm-grouped_operation-all"
 '(& (op_args-p)
     (eqc-tempo-include tempo-template-eqc-fsm-grouped_operation-set) ))

;; Template for a grouped operation with op_name set.
(tempo-define-template "eqc-fsm-grouped_operation-set"
  '(& (eqc-tempo-include tempo-template-eqc-fun-grp_pre_3) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_args_fsm) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_pre_4) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_op) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_next_fsm) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_post_fsm) n
;      (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_blocking_fsm)) n
;      (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_adapt_fsm))
;      (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_features_fsm)) n
;      (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_dynpre_fsm)) n
      n
      )
)

;; Common pre-/post-conditions
(eqc-fun-def "fsm-common_prefilter"
 '("%% @doc General command filter, checked before a command is generated." > n)
 '( "-spec command_precondition_common(From, To, S, Cmd) -> boolean()" n
  "    when From :: eqc_fsm:state_name()," n
  "         To   :: eqc_fsm:state_name()," n
  "         S    :: eqc_statem:symbolic_state()," n
  "         Cmd  :: atom()." n)
 '("command_precondition_common(_From, _To, _S, _Cmd) ->" > n
   "true." > n)
 "fcommand_pre_common")

(eqc-fun-def "fsm-common_precondition"
 '("%% @doc General precondition, applied *before* specialized preconditions." > n)
 '( "-spec precondition_common(From, To, S, Call) -> boolean()" n
   "    when From :: eqc_fsm:state_name()," n
   "         To   :: eqc_fsm:state_name()," n
   "         S    :: eqc_statem:symbolic_state()," n
   "         Call :: eqc_statem:call()." n)
 '("precondition_common(_From, _To, _S, _Call) ->" > n
   "true." > n)
 "fpre_common")

(eqc-fun-def "fsm-common_postcondition"
 '("%% @doc General postcondition, applied *after* specialized postconditions." > n)
 '( "-spec postcondition_common(From, To, S, Call, Res) -> boolean()" n
   "    when From :: eqc_fsm:state_name()," n
   "         To   :: eqc_fsm:state_name()," n
   "         S    :: eqc_statem:dynamic_state()," n
   "         Call :: eqc_statem:call()," n
   "         Res  :: term()." n)
 '("postcondition_common(_From, _To, _S, _Call, _Res) ->" > n
   "true." > n)
 "fpost_common")


(tempo-define-template "eqc-fsm-grouped_operation"
 '(& (op_args-p)
     (eqc-tempo-include tempo-template-eqc-fun-grp_args_fsm) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_op) n)
 "fop")

(tempo-define-template "eqc-add-args_fsm"
 `(& (tempo-save-named 'op_name (eqc-find-op-name 'down))
     (eqc-tempo-include tempo-template-eqc-fun-grp_args_fsm))
 "fargs")

(tempo-define-template "eqc-add-pre_3"
 `(& (tempo-save-named 'op_name (eqc-find-op-name 'down))
     (eqc-tempo-include tempo-template-eqc-fun-grp_pre_3))
 "pre3")

(tempo-define-template "eqc-add-pre_4"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params)
     (eqc-tempo-include tempo-template-eqc-fun-grp_pre_4))
 "pre4")

(tempo-define-template "eqc-add-next_fsm"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_next_fsm))
 "fnext")

(tempo-define-template "eqc-add-post_fsm"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_post_fsm))
 "fpost")

(tempo-define-template "eqc-add-blocking_fsm"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_blocking_fsm))
 "fblocking")

(tempo-define-template "eqc-add-adapt_fsm"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_adapt_fsm))
 "fadapt")

(tempo-define-template "eqc-add-features_fsm"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_features_fsm))
 "ffeatures")

(tempo-define-template "eqc-add-dynpre_fsm"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_dynpre_fsm))
 "fdynpre")

(eqc-fun-def "new_state-grouped"
 '( "%% @doc Definition of state transitions. Each state is represented by a" > n
    "%%      function, listing the transitions from that state, together with" > n
    "%%      the operation to perform each transition." > n)
 '( "-spec " (s new_state) "() -> list({eqc_fsm:state_name(), atom()})." n)
 '( (s new_state) "() ->" > n
    "[ %% {target_state, transition_function}" > n
    "]." > n))

(eqc-fun-def "grp_args_fsm"
 '( "%% @doc " (s op_name) "_args - Argument generator" > n)
 '( "-spec " (s op_name) "_args(From, To, S) -> eqc_gen:gen([term()])" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()." n)
 '( (s op_name) "_args(_From, _To, _S) ->" > n
    "[" ~ (s gens) "]." > n)
 "grp-args_fsm")

(eqc-fun-def "grp_pre_3"
 '( "%% @doc " (s op_name) "_pre/3 - Precondition for generation" > n)
 '( "-spec " (s op_name) "_pre(From, To, S) -> boolean()" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()." n)
 '( (s op_name) "_pre(_From, _To, _S) ->" > n
    "  true." > n ))

(eqc-fun-def "grp_pre_4"
 '( "%% @doc " (s op_name) "_pre/4 - Precondition for " (s op_name) > n)
 '( "-spec " (s op_name) "_pre(From, To, S, Args) -> boolean()" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()," n
     "        Args :: [term()]." n)
 '( (s op_name) "_pre(_From, _To, _S, " (s params) ") ->" > n
    "  true." > n))

(eqc-fun-def "grp_next_fsm"
 '( "%% @doc " (s op_name) "_next - Next state function" > n)
 '( "-spec " (s op_name) "_next(From, To, S, V, Args) -> eqc_statem:symbolic_state()" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()," n
     "        V    :: term()," n
     "        Args :: [term()]." n)
 '( (s op_name) "_next(_From, _To, S, _Value, " (s params) ") ->" > n
    " S." > n))

(eqc-fun-def "grp_post_fsm"
 '( "%% @doc " (s op_name) "_post - Postcondition for " (s op_name) > n)
 '( "-spec " (s op_name) "_post(From, To, S, Args, Res) -> true | term()" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()," n
     "        Args :: [term()]," n
     "        Res  :: term()." n)
 '( (s op_name) "_post(_From, _To, _S, " (s params) ", _Res) ->" > n
    "  true." > n))

(eqc-fun-def "grp_blocking_fsm"
 '( "%% @doc " (s op_name) "_blocking - Is the operation blocking in this State" > n)
 '( "-spec " (s op_name) "_blocking(From, To, S, Args) -> boolean()" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()," n
     "        Args :: [term()]." n)
 '( (s op_name) "_blocking(_From, _To, _S, " (s params) ") ->" > n
    "  false." > n))

(eqc-fun-def "grp_adapt_fsm"
 '( "%% @doc " (s op_name) "_adapt - How to adapt a call in this State" > n)
 '( "-spec " (s op_name) "_adapt(From, To, S, Args) -> boolean()" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()," n
     "        Args :: [term()]." n)
 '( (s op_name) "_adapt(_From, _To, _S, " (s params) ") ->" > n
    "  false." > n))

(eqc-fun-def "grp_features_fsm"
 '( "%% @doc " (s op_name) "_features - Collects a list of features of this call with these arguments." > n)
 '( "-spec " (s op_name) "_features(From, To, S, Args, Res) -> list(any())" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:dynamic_state()," n
     "        Args :: [term()]" n
     "        Res  :: term()." n)
 '( (s op_name) "_features(_From, _To, _S, " (s params) ", _Res) ->" > n
    "  []." > n))

(eqc-fun-def "grp_dynpre_fsm"
 '( "%% @doc " (s op_name) "_dynamicpre - Dynamic precondition for " (s op_name) > n)
 '( "-spec " (s op_name) "_dynamicpre(From, To, S, Args) -> boolean()" n
     "   when From :: eqc_fsm:state_name()," n
     "        To   :: eqc_fsm:state_name()," n
     "        S    :: eqc_statem:symbolic_state()," n
     "        Args :: [term()]." n)
 '( (s op_name) "_dynamicpre(_From, _To, _S, " (s params) ") ->" > n
    "  true." > n))

(eqc-fun-def "fsm-weight-grouped"
 '( "%% @doc <i> Optional callback</i> Weight for transition(s)." > n
    "%%      Specify how often each transition should be generated" > n)
 '( "-spec weight(From, To, Cmd, Args) -> integer()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name()," n
    "         Cmd  :: atom()," n
    "         Args :: [term()]." n)
 '( "weight(_From, _To, _Cmd, _Args) ->" > n
     > "1." n))

(eqc-fun-def "fsm-priority-grouped"
 '( "%% @doc <i> Optional callback</i> Priority for transition(s)." > n
    "%%      Similar to weight, but used in case automatic weights are computed." > n)
 '( "-spec priority(From, To, Cmd, Args) -> integer()" n
    "    when From :: eqc_fsm:state_name()," n
    "         To   :: eqc_fsm:state_name()," n
    "         Cmd  :: atom()," n
    "         Args :: [term()]." n)
 '( "priority(_From, _To, _Cmd, _Args) ->" > n
     > "1." n))


