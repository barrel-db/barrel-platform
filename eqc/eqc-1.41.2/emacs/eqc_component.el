;; QuickCheck mode definitions for eqc_cluster

(defvar eqc-menu-eqc_component
  (eqc-module-menu-prepare
   "Component specs"
   '(["Complete eqc_component spec" tempo-template-eqc-module-eqc_component]
     ["--" 'ignore]
     ["Full eqc_component module header" tempo-template-eqc-header-eqc_component]
     ["--" 'ignore]
     ["Include eqc_component" tempo-template-eqc-include-eqc_component]
     ["Default Component Property" tempo-template-eqc-prop-eqc_component]
     ["New component operation" tempo-template-eqc-component_operation]
     ["New component operation - All" tempo-template-eqc-component_operation-all]
     ["New grouped operation - Interactive" tempo-template-eqc-component_operation-interactive]
     ["--" 'ignore]
     ["Navigation goto-or-create callback " eqc-goto-or-create]
     ["Navigation go-back to ?APPLY" eqc-go-back]
     ["--" 'ignore]
     ["Callback command_precondition_common" tempo-template-eqc-fun-common_prefilter]
     ["Callback precondition_common" tempo-template-eqc-fun-common_precondition]
     ["Callback postcondition_common" tempo-template-eqc-fun-common_postcondition-component]
     ["Callback call_features_common" tempo-template-eqc-fun-common_call_features]
     ["Callback process_common" tempo-template-eqc-fun-common_process]
     ["--" 'ignore]
     ["Callback initial_state" tempo-template-eqc-fun-initial_state]
     ["Callback api_spec" tempo-template-eqc-fun-api_spec-component]
     ["Callback weight" tempo-template-eqc-fun-weight-grouped]
     ["--" 'ignore]
     ["Operation - add pre_1" tempo-template-eqc-add-pre_1]
     ["Operation - add pre_2" tempo-template-eqc-add-pre_2]
     ["Operation - add callouts" tempo-template-eqc-add-callouts]
     ["Operation - add process" tempo-template-eqc-add-process]
     ["Operation - add next" tempo-template-eqc-add-next]
     ["Operation - add post" tempo-template-eqc-add-post]
     ["Operation - add return" tempo-template-eqc-add-return]
     ["Operation - add callers" tempo-template-eqc-add-callers]
     ["Operation - add blocking" tempo-template-eqc-add-blocking]
     ["Operation - add features" tempo-template-eqc-add-features]
     ["Operation - add adapt" tempo-template-eqc-add-adapt]
     ["Operation - add dynamic precond." tempo-template-eqc-add-dynpre]
     ["--" 'ignore]
     ["Symbolic function call" tempo-template-eqc-symbolic-call]
     ["Duplicate function clause" duplicate-function-clause]
     ["Duplicate function clause header" duplicate-function-clause-header]
;     ["--" 'ignore]
;     ["Clone property to parallel property" change_statem_property_to_parallel]
;     ["Parallel property" tempo-template-eqc-prop-eqc_statem-par]
     )
   (eqc-menu-list-expand eqc-component-autogen-fun-menu-items))
  )

;; Component property
(eqc-prop-def "eqc_component"
 '( (default-p "Property name (prop_NAME)" (erlang-get-module-from-file-name) 'prop) )
 '( "%% @doc Default generated property" > n )
 '( "-spec prop_" (s prop) "() -> eqc:property()." > n )
 '( (default-p "Module for commands" "?MODULE" 'module)
    "prop_" (s prop) "() ->" > n
    (eqc-indent-string 1) "?SETUP(" n
    (eqc-indent-string 2) "fun() ->" n
    "%% Setup mocking, etc." > n
    "eqc_mocking:start_mocking(api_spec())," > n
    "%% Return the teardwown function" > n
    "fun() -> ok end" > n
    "end," > n
    (eqc-indent-string 1) "?FORALL(Cmds, commands(" (s module) ")," n
    (eqc-indent-string 1) "begin" n
    "{H, S, Res} = run_commands(Cmds)," > n
    "check_command_names(Cmds," > n
    (eqc-indent-string 3) "measure(length, commands_length(Cmds)," n
    (eqc-indent-string 4) "pretty_commands(" (s module) ", Cmds, {H, S, Res}," n
    (eqc-indent-string 4 (length "pretty_commands(")) (if (is-region-active) 'r> "Res == ok")
    ")))" n
    "end))." > n)
  "component_property")

;; Complete eqc_component spec skeleton
(tempo-define-template  "eqc-module-eqc_component"
 '((eqc-tempo-include tempo-template-eqc-header-eqc_component) n

   (mk-comment-h1 "State") > n
   (ask-record 'rec nil)

   (eqc-tempo-include tempo-template-eqc-fun-initial_state) n
   (mk-comment-h1 "Common pre-/post-conditions") > n
   (eqc-tempo-include tempo-template-eqc-fun-common_prefilter) n
   (eqc-tempo-include tempo-template-eqc-fun-common_precondition) n
   (eqc-tempo-include tempo-template-eqc-fun-common_postcondition-component) n

   (mk-comment-h1 "Operations") > n n
   (default-p "Name of (first) operation" "op" 'op_name)
   (tempo-save-named 'args "") ;Use default here too many questions
   (tempo-save-named 'params "_Args") ;Use default here too many questions
   (mk-op-comment 'op_name)
   (eqc-tempo-include tempo-template-eqc-component_operation-all-set)
   " %% --- ... more operations" > n
   n
   (mk-comment-h1 "Property") > n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-invariant)) n
   (eqc-tempo-include tempo-template-eqc-fun-weight-grouped) n

   (tempo-save-named 'module "?MODULE")
   (tempo-save-named 'prop (erlang-get-module-from-file-name))
   (eqc-tempo-include tempo-template-eqc-prop-eqc_component) n
   (eqc-tempo-include tempo-template-eqc-fun-morebugs2) n
   (eqc-tempo-include tempo-template-eqc-fun-morebugs3) n
   (eqc-tempo-include tempo-template-eqc-fun-morebugs4) n
   (mk-comment-h1 "API-spec") > n
   (eqc-tempo-include tempo-template-eqc-fun-api_spec-component)
   )
 "eqc_component"
 )

(tempo-define-template "eqc-component_operation-interactive"
 '(& (op_args-p)
     (op_ask "pre/1" 'tempo-template-eqc-fun-grp_pre_1)

     (eqc-tempo-include tempo-template-eqc-fun-grp_args) n

     (op_ask "pre/2" 'tempo-template-eqc-fun-grp_pre_2)

     (eqc-tempo-include tempo-template-eqc-fun-grp_op) n

     (op_ask "callouts" 'tempo-template-eqc-fun-grp_callouts)
     (op_ask "process"  'tempo-template-eqc-fun-grp_process)
     (op_ask "next"     'tempo-template-eqc-fun-grp_next)
     (op_ask "return"   'tempo-template-eqc-fun-grp_return)
     (op_ask "post"     'tempo-template-eqc-fun-grp_post)
     (op_ask "callers"  'tempo-template-eqc-fun-grp_callers)
     (op_ask "features" 'tempo-template-eqc-fun-grp_features)
     (op_ask "adapt"    'tempo-template-eqc-fun-grp_adapt)
     (op_ask "blocking" 'tempo-template-eqc-fun-grp_blocking)
     (op_ask "dynpre"   'tempo-template-eqc-fun-grp_dynpre)
  )
 "ciop")

(tempo-define-template "eqc-component_operation-all"
 '(& (op_args-p)
     (eqc-tempo-include tempo-template-eqc-component_operation-all-set) ))

(tempo-define-template  "eqc-component_operation-all-set"
 '(& (eqc-tempo-include tempo-template-eqc-fun-grp_pre_1) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_args) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_pre_2) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_op) n
     ; (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_callers)) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_callouts) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_process) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_next) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_post) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_return) n
     ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_blocking)) n
     ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_features)) n
     ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_adapt)) n
     ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_dynpre)) n
     n
     ) )

(tempo-define-template "eqc-component_operation"
 '(& (op_args-p)
     (eqc-tempo-include tempo-template-eqc-fun-grp_args) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_op) n)
 "cop")

(tempo-define-template "eqc-add-callers"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (eqc-tempo-include tempo-template-eqc-fun-grp_callers))
 "callers")

(tempo-define-template "eqc-add-callouts"
 `(& (if (null eqc-current-op-name)
       (tempo-save-named 'op_name (eqc-find-op-name))
       (tempo-save-named 'op_name eqc-current-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_callouts))
 "callouts")

(tempo-define-template "eqc-add-process"
 `(& (if (null eqc-current-op-name)
       (tempo-save-named 'op_name (eqc-find-op-name))
       (tempo-save-named 'op_name eqc-current-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_process))
 "process")

;; Common postcondition
(eqc-fun-def "common_postcondition-component"
 '( "%% @doc General postcondition, applied *after* specialized postconditions." > n)
 '( "-spec postcondition_common(S, Call, Res) -> true | term()" n
    "    when S    :: eqc_statem:dynamic_state(), "  n
    "         Call :: eqc_statem:call()," n
    "         Res  :: term()." n)
 '( "postcondition_common(S, Call, Res) ->" > n
    "eq(Res, return_value(S, Call)). %% Check all return values" > n))

(eqc-fun-def "api_spec-component"
 '( "%% @doc API specification for mocked components" > n)
 '( "-spec api_spec() -> #api_spec{}." > n)
 '( "api_spec() -> #api_spec{ language = erlang, mocking = eqc_mocking, modules = [] }." > n ))

(eqc-fun-def "grp_callers"
 '( "%% @doc " (s op_name) "_callers - Which modules are allowed to call this operation. Default: [anyone]" > n)
 '( "-spec " (s op_name) "_callers() -> [atom()]." > n)
 '( (s op_name) "_callers() ->" > n
    "  [anyone]." > n n))

