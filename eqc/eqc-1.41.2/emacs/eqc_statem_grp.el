;; QuickCheck mode definitions for eqc_statem Grouped style

(defvar eqc-menu-eqc_statem_grp
  (eqc-module-menu-prepare
   "State Machine specs - Grouped style"
   '(["Complete eqc_statem spec" tempo-template-eqc-module-eqc_statem-grouped]
     ["--" 'ignore]
     ["Full statem module header" tempo-template-eqc-header-eqc_statem]
     ["--" 'ignore]
     ["Include eqc_statem" tempo-template-eqc-include-eqc_statem]
     ["State Machine Property" tempo-template-eqc-prop-eqc_statem]
     ["New operation" tempo-template-eqc-grouped_operation]
     ["New operation - All callbacks" tempo-template-eqc-grouped_operation-all]
     ["New operation - Interactive" tempo-template-eqc-grouped_operation-interactive]
     ["--" 'ignore]
     ["Callback command_precondition_common" tempo-template-eqc-fun-common_prefilter]
     ["Callback precondition_common" tempo-template-eqc-fun-common_precondition]
     ["Callback postcondition_common" tempo-template-eqc-fun-common_postcondition]
     ["Callback call_features_common" tempo-template-eqc-fun-common_call_features]
     ["--" 'ignore]
     ["Callback initial_state" tempo-template-eqc-fun-initial_state]
     ["Callback invariant" tempo-template-eqc-fun-invariant]
     ["Callback weight" tempo-template-eqc-fun-weight-grouped]
     ["--" 'ignore]
     ["Operation - add args" tempo-template-eqc-add-args]
     ["Operation - add pre_1" tempo-template-eqc-add-pre_1]
     ["Operation - add pre_2" tempo-template-eqc-add-pre_2]
     ["Operation - add next" tempo-template-eqc-add-next]
     ["Operation - add post" tempo-template-eqc-add-post]
     ["Operation - add blocking" tempo-template-eqc-add-blocking]
     ["Operation - add features" tempo-template-eqc-add-features]
     ["Operation - add adapt" tempo-template-eqc-add-adapt]
     ["Operation - add dynamic precond." tempo-template-eqc-add-dynpre]
     ["--" 'ignore]
     ["Clone property to parallel property" change_statem_property_to_parallel]
     ["Parallel property" tempo-template-eqc-prop-eqc_statem-par]
     ["--" 'ignore]
     )
   (eqc-menu-list-expand eqc-statem-autogen-fun-menu-items))
  )

;; Complete eqc_statem spec skeleton Grouped style
(tempo-define-template "eqc-module-eqc_statem-grouped"
 '(
   (eqc-tempo-include tempo-template-eqc-header-eqc_statem)
   n

   (mk-comment-h1 "State and state functions") > n
   (ask-record 'rec nil)
   (eqc-tempo-include tempo-template-eqc-fun-initial_state) n

   (mk-comment-h1 "Generators") > n n

   (mk-comment-h1 "Common pre-/post-conditions") n
   (eqc-tempo-include tempo-template-eqc-fun-common_prefilter) n
   (eqc-tempo-include tempo-template-eqc-fun-common_precondition) n
   (eqc-tempo-include tempo-template-eqc-fun-common_postcondition) n

   (mk-comment-h1 "Operations") > n n
   (default-p "Name of (first) operation" "op" 'op_name)
   (tempo-save-named 'args "") ;Use default here too many questions
   (tempo-save-named 'params "_Args") ;Use default here too many questions
   (mk-op-comment 'op_name)
   (eqc-tempo-include tempo-template-eqc-grouped_operation-set)

   "%% --- ... more operations" n n

   (mk-comment-h1 "Property") > n
   ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-invariant)) n
   ;(eqc-tempo-include tempo-template-eqc-fun-weight-grouped) n

   ;Use default here too many questions
   (tempo-save-named 'module "?MODULE")
   (tempo-save-named 'prop (erlang-get-module-from-file-name))
   (eqc-tempo-include tempo-template-eqc-prop-eqc_statem) n
   (eqc-tempo-include tempo-template-eqc-fun-morebugs2) n
   (eqc-tempo-include tempo-template-eqc-fun-morebugs3) n
   (eqc-tempo-include tempo-template-eqc-fun-morebugs4)
   )
 "eqc_statem"
 )

;; Common pre-/post-conditions
(eqc-fun-def "common_prefilter"
  '("%% @doc General command filter, checked before a command is generated." > n)
 '( "-spec command_precondition_common(S, Cmd) -> boolean()" n
    "    when S    :: eqc_statem:symbolic_state()," n
    "         Cmd  :: atom()." n)
  '("command_precondition_common(_S, _Cmd) ->" > n
    "true." > n)
  "command_pre_common")

(eqc-fun-def "common_precondition"
 '("%% @doc General precondition, applied *before* specialized preconditions." > n)
 '( "-spec precondition_common(S, Call) -> boolean()" n
   "    when S    :: eqc_statem:symbolic_state()," n
   "         Call :: eqc_statem:call()." n)
 '("precondition_common(_S, _Call) ->" > n
   "true." > n)
 "pre_common")

(eqc-fun-def "common_postcondition"
 '("%% @doc General postcondition, applied *after* specialized postconditions." > n)
 '( "-spec postcondition_common(S, Call, Res) -> true | term()" n
   "    when S    :: eqc_statem:dynamic_state()," n
   "         Call :: eqc_statem:call()," n
   "         Res  :: term()." n)
 '("postcondition_common(_S, _Call, _Res) ->" > n
   "true." > n)
 "post_common")

(eqc-fun-def "common_call_features"
 '("%% @doc General call_features, added to specialized call_feature." > n)
 '( "-spec call_features_common(S, Call, Res) -> [any()]" n
   "    when S    :: eqc_statem:symbolic_state()," n
   "         Call :: eqc_statem:call()," n
   "         Res  :: term()." n)
 '("call_features_common(_S, _Call, _Res) ->" > n
   "[]." > n)
 "call_features_common")

(eqc-fun-def "common_process"
 '("%% @doc General process, used if no specialized process is given." > n)
 '( "-spec process_common(S, Call) -> default | root | worker | spawn | symbolic_pid()" n
   "    when S    :: eqc_statem:symbolic_state()," n
   "         Call :: eqc_statem:call()." n)
 '("process_common(_S, _Call) ->" > n
   "default." > n)
 "process_common")

;; Template for weight function (grouped statem)
(eqc-fun-def "weight-grouped"
 '( "%% @doc weight/2 - Distribution of calls" > n)
 '( "-spec weight(S, Cmd) -> integer()" n
     "    when S   :: eqc_statem:symbolic_state()," n
     "         Cmd :: atom()." n)
 '( (unless (null (tempo-lookup-named 'op_name))
      '(l "weight(_S, " (s op_name) ") -> 1;" > n ) )
    "weight(_S, _Cmd) -> 1." > n )
 "grp-weight")

(tempo-define-template "eqc-grouped_operation-interactive"
 '(& (op_args-p)
     (op_ask "pre/1" 'tempo-template-eqc-fun-grp_pre_1)

     (eqc-tempo-include tempo-template-eqc-fun-grp_args) n

     (op_ask "pre/2" 'tempo-template-eqc-fun-grp_pre_2)

     (eqc-tempo-include tempo-template-eqc-fun-grp_op) n

     (op_ask "next"     'tempo-template-eqc-fun-grp_next)
     (op_ask "post"     'tempo-template-eqc-fun-grp_post)
     (op_ask "blocking" 'tempo-template-eqc-fun-grp_blocking)
     (op_ask "features" 'tempo-template-eqc-fun-grp_features)
     (op_ask "adapt"    'tempo-template-eqc-fun-grp_adapt)
     (op_ask "dynpre"   'tempo-template-eqc-fun-grp_dynpre)
  )
 "iop")


;; Template for a grouped operation.
(tempo-define-template  "eqc-grouped_operation-all"
 '(& (op_args-p)
     (eqc-tempo-include tempo-template-eqc-grouped_operation-set) )
 "op")

;; Template for a grouped operation with op_name set.
(tempo-define-template "eqc-grouped_operation-set"
  '(& (eqc-tempo-include tempo-template-eqc-fun-grp_pre_1) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_args) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_pre_2) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_op) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_next) n
      (eqc-tempo-include tempo-template-eqc-fun-grp_post) n
      ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_blocking)) n
      ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_features)) n
      ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_adapt)) n
      ;(eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-grp_dynpre)) n
      n
      )
)

(tempo-define-template "eqc-grouped_operation"
 '(& (op_args-p)
     (eqc-tempo-include tempo-template-eqc-fun-grp_args) n
     (eqc-tempo-include tempo-template-eqc-fun-grp_op) n)
 "-op")

(tempo-define-template "eqc-add-args"
 `(& (tempo-save-named 'op_name (eqc-find-op-name 'down))
     (eqc-tempo-include tempo-template-eqc-fun-grp_args))
 "args")

(tempo-define-template "eqc-add-command"
 `(& (tempo-save-named 'op_name (eqc-find-op-name 'down))
     (eqc-tempo-include tempo-template-eqc-fun-grp_command))
 "command")

(tempo-define-template "eqc-add-pre_1"
 `(& (tempo-save-named 'op_name (eqc-find-op-name 'down))
     (eqc-tempo-include tempo-template-eqc-fun-grp_pre_1))
 "pre1")

(tempo-define-template "eqc-add-pre_2"
 `(& (if (null eqc-current-op-name)
       (tempo-save-named 'op_name (eqc-find-op-name))
       (tempo-save-named 'op_name eqc-current-op-name))
     (set-params)
     (eqc-tempo-include tempo-template-eqc-fun-grp_pre_2))
 "pre2")

(tempo-define-template "eqc-add-next"
 `(& (if (null eqc-current-op-name)
       (tempo-save-named 'op_name (eqc-find-op-name))
       (tempo-save-named 'op_name eqc-current-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_next))
 "next")

(tempo-define-template "eqc-add-post"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_post))
 "post")

(tempo-define-template "eqc-add-return"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_return))
 "return")

(tempo-define-template "eqc-add-blocking"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_blocking))
 "blocking")

(tempo-define-template "eqc-add-adapt"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_adapt))
 "adapt")

(tempo-define-template "eqc-add-features"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_features))
 "features")

(tempo-define-template "eqc-add-dynpre"
 `(& (tempo-save-named 'op_name (eqc-find-op-name))
     (set-params nil)
     (eqc-tempo-include tempo-template-eqc-fun-grp_dynpre))
 "dynpre")

;; Templates for individual grouped operations
(eqc-fun-def "grp_command"
 '( "%% @doc " (s op_name) "_command - Command generator" > n)
 '( "-spec " (s op_name) "_command(S) -> eqc_gen:gen(eqc_statem:call())" n
     "    when S :: eqc_statem:symbolic_state()." n)
 '( (s op_name) "_command(_S) ->" > n
    "{call, ?MODULE, " (s op_name) ", [" ~ "]}." > n)
 "grp-cmd"
)

(eqc-fun-def "grp_args"
 '( "%% @doc " (s op_name) "_args - Argument generator" > n)
 '( "-spec " (s op_name) "_args(S :: eqc_statem:symbolic_state()) -> eqc_gen:gen([term()])."  n)
 '( (s op_name) "_args(_S) ->" > n "[" ~ (s gens) "]." > n)
 "grp-args"
)

(eqc-fun-def "grp_op"
 '( "%% @doc " (s op_name) " - The actual operation" > n)
 '()
 '( (s op_name)  "(" (s args) ") ->" > n "ok." > n)
 "grp-op"
)

(eqc-fun-def "grp_pre_1"
 '( "%% @doc " (s op_name) "_pre/1 - Precondition for generation" > n)
 '( "-spec " (s op_name) "_pre(S :: eqc_statem:symbolic_state()) -> boolean()." > n)
 '( (s op_name) "_pre(_S) ->" > n
    "  true." > n ))

(eqc-fun-def "grp_pre_2"
 '( "%% @doc " (s op_name) "_pre/2 - Precondition for " (s op_name) > n)
 '( "-spec " (s op_name) "_pre(S, Args) -> boolean()" n
     "    when S    :: eqc_statem:symbolic_state()," n
     "         Args :: [term()]." > n)
 '( (s op_name) "_pre(_S, " (s params) ") ->" > n
    "  true." > n))

(eqc-fun-def "grp_next"
 '( "%% @doc " (s op_name) "_next - Next state function" > n)
 '( "-spec " (s op_name) "_next(S, Var, Args) -> NewS" > n
     "    when S    :: eqc_statem:symbolic_state() | eqc_state:dynamic_state()," n
     "         Var  :: eqc_statem:var() | term()," n
     "         Args :: [term()]," n
     "         NewS :: eqc_statem:symbolic_state() | eqc_state:dynamic_state()." n)
 '( (s op_name) "_next(S, _Value, " (s params) ") ->" > n
    " S." > n))

(eqc-fun-def "grp_post"
 '( "%% @doc " (s op_name) "_post - Postcondition for " (s op_name) > n)
 '( "-spec " (s op_name) "_post(S, Args, Res) -> true | term()"  n
     "    when S    :: eqc_state:dynamic_state()," n
     "         Args :: [term()]," n
     "         Res  :: term()." n)
 '( (s op_name) "_post(_S, " (s params) ", _Res) ->" > n
    "  true." > n))

(eqc-fun-def "grp_callouts"
 '( "%% @doc " (s op_name) "_callouts - Callouts for " (s op_name) > n)
 '( "-spec " (s op_name) "_callouts(S, Args) -> eqc_gen:gen(eqc_component:callout())" n
     "    when S    :: eqc_statem:symbolic_state()," n
     "         Args :: [term()]." > n)
 '( (s op_name) "_callouts(_S, " (s params) ") ->" > n
    "  ?EMPTY." > n))

(eqc-fun-def "grp_process"
 '( "%% @doc " (s op_name) "_process - Callouts for " (s op_name) > n)
 '( "-spec " (s op_name) "_process(S, Args) -> Res" n
     "    when S    :: eqc_statem:symbolic_state()," n
     "         Args :: [term()]," n
     "         Res  :: default | root | worker | spawn | symbolic_pid()." n)
 '( (s op_name) "_process(_S, " (s params) ") ->" > n
    "  default." > n))

(eqc-fun-def "grp_return"
 '( "%% @doc " (s op_name) "_return - Return value for " (s op_name) > n)
 '( "-spec " (s op_name) "_return(S, Args) -> term()" n
     "    when S    :: eqc_statem:symbolic_state()," n
     "         Args :: [term()]." > n)
 '( (s op_name) "_return(_S, " (s params) ") ->" > n
    "  ok." > n))

(eqc-fun-def "grp_blocking"
 '( "%% @doc " (s op_name) "_blocking - Is the operation blocking in this State" > n)
 '( "-spec " (s op_name) "_blocking(S, Args) -> boolean()" n
     "    when S    :: eqc_statem:symbolic_state()," n
     "         Args :: [term()]." > n)
 '( (s op_name) "_blocking(_S, " (s params) ") ->" > n
    "  false." > n))

(eqc-fun-def "grp_adapt"
 '( "%% @doc " (s op_name) "_adapt - How to adapt a call in this State" > n)
 '( "-spec " (s op_name) "_adapt(S, Args) -> boolean()" n
     "    when S    :: eqc_statem:symbolic_state()," n
     "         Args :: [term()]." > n)
 '( (s op_name) "_adapt(_S, " (s params) ") ->" > n
    "  false." > n))

(eqc-fun-def "grp_features"
 '( "%% @doc " (s op_name) "_features - Collects a list of features of this call with these arguments." > n)
 '( "-spec " (s op_name) "_features(S, Args, Res) -> list(any())" n
     "    when S    :: eqc_statem:dynmic_state()," n
     "         Args :: [term()]," n
     "         Res  :: term()." n)
 '( (s op_name) "_features(_S, " (s params) ", _Res) ->" > n
    "  []." > n))

(eqc-fun-def "grp_dynpre"
 '( "%% @doc " (s op_name) "_dynamicpre - Dynamic precondition for " (s op_name) > n)
 '( "-spec " (s op_name) "_dynamicpre(S, Args) -> boolean()" n
     "    when S    :: eqc_statem:symbolic_state()," n
     "         Args :: [term()]." > n)
 '( (s op_name) "_dynamicpre(_S, " (s params) ") ->" > n
    "  true." > n))

(defun eqc-add-metadata ()
  "Adds metadata variable to current function clause(s)"
  (interactive)
  (let (funend cont pos)
    (save-excursion
      (erlang-end-of-function)
      (setq funend (point) cont t)
      (erlang-beginning-of-function)
      (while cont
        (message "Curr pos1: %d" (point))
        (condition-case nil
            (progn
              (re-search-forward "(" funend)
              (backward-char 1)
              (setq pos (point))
              (forward-sexp)
              (backward-char 1)
              (if (= 1 (- (point) pos))
                  (progn (insert "MetaData") (setq funend (+ funend 8)))
                (progn (insert ", MetaData") (setq funend (+ funend 10)))
                )
              (erlang-beginning-of-clause)
              (setq pos (point))
              (erlang-end-of-clause)
              (erlang-end-of-clause)
              (erlang-beginning-of-clause)
              (setq cont (> (point) pos))
              )
          (error (setq cont nil))
          )
        )
      ))
)

