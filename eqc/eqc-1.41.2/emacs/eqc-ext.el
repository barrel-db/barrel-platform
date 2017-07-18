(require 'tempo)

(load "eqc-menu-tools")
(load "eqc-tempo-funs")
(load "eqc-ext-gen")
(load "eqc_properties")
(load "eqc_generators")
(load "eqc_statem_ungrp")
(load "eqc_statem_grp")
(load "eqc_fsm_ungrp")
(load "eqc_fsm_grp")
(load "eqc_component")
(load "eqc_cluster")
(load "eqc_c")
(load "eqc_temporal")
(load "eqc_mocking")

(defcustom eqc-include-docs t
  "Non-nil value means..."
  :type 'boolean
  :group 'eqc-ext)

(defcustom eqc-include-specs t
  "Non-nil value means..."
  :type 'boolean
  :group 'eqc-ext)

(defun eqc-erlang-mode-hook ()
  (eqc-mode-init)
  )

(defun eqc-menu-init ()
  (let ((menu (eqc-split-long-menu
              `("QuickCheck"
                ,eqc-menu-properties
                ,eqc-menu-generators
                ["--" 'ignore]
                ,eqc-menu-eqc_statem_grp
                ,eqc-menu-eqc_statem_ungrp
                ["--" 'ignore]
                ,eqc-menu-eqc_fsm_grp
                ,eqc-menu-eqc_fsm_ungrp
                ["--" 'ignore]
                ,eqc-menu-eqc_component
                ,eqc-menu-eqc_cluster
                ["--" 'ignore]
                ,eqc-menu-eqc_c
                ,eqc-menu-eqc_temporal
                ,eqc-menu-eqc_mocking
                ,eqc-menu-eqc_mocking_c
                ["--" 'ignore]
                ,eqc-menu-abbrevs
                ,eqc-menu-settings
                ))))
    (easy-menu-define my-menu erlang-mode-map "QuickCheck Menu" menu)
  )
)

(defvar eqc-menu-abbrevs
  ["QuickCheck abbreviations (^Q TAB)" (eqc-show-abbreviations)]
  )

(defvar eqc-menu-settings
  '("QuickCheck mode settings"
     ["Include @doc's for templates" (toggle 'eqc-include-docs)
      :style toggle :selected eqc-include-docs]
     ["Include -spec's for templates" (toggle 'eqc-include-specs)
      :style toggle :selected eqc-include-specs]
    )
  )

(defun toggle (x)
  (if (symbolp x)
      (progn
        (set x (not (symbol-value x)))
        (put x 'customized-value (list (custom-quote (eval x))))
        (customize-save-customized)
        )
    nil)
  )

(defun eqc-mode-init ()
  "Initialize"
  (setq tempo-interactive t)
  (eqc-mode-map-init)
  (eqc-menu-init)
  )

(defun eqc-mode-map-init ()
  (local-set-key (kbd "\C-q TAB") 'tempo-complete-tag)
  (local-set-key (kbd "\C-q \C-c TAB") 'eqc-mk-fancy-comment-h1)
  (local-set-key (kbd "\C-q \C-c 1") 'eqc-mk-fancy-comment-h1)
  (local-set-key (kbd "\C-q \C-c 2") 'eqc-mk-fancy-comment-h2)
  (local-set-key (kbd "\C-q \C-c 3") 'eqc-mk-fancy-comment-h3)
  (local-set-key (kbd "\C-q \C-c 0") 'eqc-mk-fancy-comment-op)
  (if (symbolp 'erlang-mode-map)
      (let ((map erlang-mode-map))
        (define-key map "\C-q TAB"  'tempo-complete-tag)
        (define-key map "\C-qf"     'tempo-template-eqc-macro-prop-forall)
        (define-key map "\C-qi"     'tempo-template-eqc-macro-prop-implies)
        (define-key map "\C-ql"     'tempo-template-eqc-macro-gen-let)
        (define-key map "\C-qw"     'tempo-template-eqc-macro-prop-whenfail)
        (define-key map "\C-qh"     'tempo-template-eqc-full-header)
        (define-key map "\C-qc"     'tempo-template-eqc-symbolic-call)
        (define-key map "\C-q\C-fc" 'duplicate-function-clause)
        (define-key map "\C-q\C-fh" 'duplicate-function-clause-header)
        (define-key map "\C-q\C-su" 'tempo-template-eqc-module-eqc_statem-ungrouped)
        (define-key map "\C-q\C-sg" 'tempo-template-eqc-module-eqc_statem-grouped)
        (define-key map "\C-q\C-si" 'tempo-template-eqc-grouped_operation-interactive)
        (define-key map "\C-q\C-so" 'tempo-template-eqc-grouped_operation)
        (define-key map "\C-q\C-sa" 'tempo-template-eqc-grouped_operation-all)
        (define-key map "\C-q\C-fu" 'tempo-template-eqc-module-eqc_fsm)
        (define-key map "\C-q\C-fg" 'tempo-template-eqc-module-eqc_fsm-grouped)
        (define-key map "\C-q\C-fi" 'tempo-template-eqc-fsm-grouped_operation-interactive)
        (define-key map "\C-q\C-fo" 'tempo-template-eqc-fsm-grouped_operation)
        (define-key map "\C-q\C-fa" 'tempo-template-eqc-fsm-grouped_operation-all)
        (define-key map "\C-q\C-cg" 'tempo-template-eqc-module-eqc_component)
        (define-key map "\C-q\C-ci" 'tempo-template-eqc-component_operation-interactive)
        (define-key map "\C-q\C-co" 'tempo-template-eqc-component_operation)
        (define-key map "\C-q\C-ca" 'tempo-template-eqc-component_operation-all)
        (define-key map "\C-qg"     'eqc-goto-or-create)
        (define-key map "\C-qb"     'eqc-go-back)
        (define-key map "\C-q\C-ae" 'tempo-template-eqc-fun-api_spec-erlang)
        (define-key map "\C-q\C-ac" 'tempo-template-eqc-fun-api_spec-c)
        (define-key map "\C-q\C-af" 'tempo-template-eqc-fun-api_rec_fun-erlang)
        (define-key map "\C-q\C-ap" 'tempo-template-eqc-fun-api_rec_fun-c)
        (define-key map "\C-q\C-aa" 'tempo-template-eqc-fun-api_rec_arg-c)
        )
    nil)
  "Keymap used by QuickCheck mode"
  )

;;;;;;;
;; General functionality
;;;;;;;

;; Headers, Includes and Module definitions
(defun eqc-def-include (name file)
  "Meta function for defining include"
  (let ((modname (if (null name) "" (concat "_" name)))
        (tabname (if (null name) "inc" (concat "inc-" name))))
    (tempo-define-template
     (concat "eqc-include-eqc" modname)
     `(& "-include_lib(\"eqc/include/" ,file "\")." > n)
     tabname)
  ))

;; defines eqc-include-eqc, etc.
(eqc-def-include nil         "eqc.hrl")
(eqc-def-include "statem"    "eqc_statem.hrl")
(eqc-def-include "component" "eqc_component.hrl")
(eqc-def-include "cluster"   "eqc_cluster.hrl")
(eqc-def-include "mocking"   "eqc_mocking.hrl")
(eqc-def-include "fsm"       "eqc_fsm.hrl")
(eqc-def-include "c"         "eqc_c.hrl")
(eqc-def-include "temporal"  "eqc_temporal.hrl")
(eqc-def-include "mocking"   "eqc_mocking.hrl")
(eqc-def-include "mocking_c" "eqc_mocking_c.hrl")

;; -compile(export_all).
(tempo-define-template
 "eqc-export_all" '(& "-compile(export_all)." > n)
 "exp_all")

(defun eqc-erlang-skel-normal-header ()
  (if (not erlang-skel-vc)
      (butlast erlang-skel-normal-header 1)
    erlang-skel-normal-header))

;; defines full headers
(defun eqc-mod-header (name includes)
  "Meta function for defining module headers"
  (let ((modname (if (null name) "" (concat "_" name)))
        (tabname (if (null name) "hdr" (concat "hdr-" name))))
    (tempo-define-template
     (concat "eqc-header-eqc" modname)
     (append '(& (eqc-tempo-include (eqc-erlang-skel-normal-header))
                 (eqc-tempo-include tempo-template-eqc-include-eqc))
             includes
             '(n (eqc-tempo-include tempo-template-eqc-export_all)))
     tabname)
  ))

(eqc-mod-header nil '())
(eqc-mod-header "statem"    '((eqc-tempo-include tempo-template-eqc-include-eqc_statem)))
(eqc-mod-header "component" '((eqc-tempo-include tempo-template-eqc-include-eqc_component)))
(eqc-mod-header "cluster"   '((eqc-tempo-include tempo-template-eqc-include-eqc_cluster)))
(eqc-mod-header "mocking"   '((eqc-tempo-include tempo-template-eqc-include-eqc_mocking)))
(eqc-mod-header "fsm"       '((eqc-tempo-include tempo-template-eqc-include-eqc_fsm)))
(eqc-mod-header "c"         '((eqc-tempo-include tempo-template-eqc-include-eqc_c)))
(eqc-mod-header "temporal"  '((eqc-tempo-include tempo-template-eqc-include-eqc_temporal)))
(eqc-mod-header "mocking"   '((eqc-tempo-include tempo-template-eqc-include-eqc_mocking)))
(eqc-mod-header "mocking_c" '((eqc-tempo-include tempo-template-eqc-include-eqc_mocking_c)))

(tempo-define-template "eqc-symbolic-call"
 '("{call," (default-p "Module name" "?MODULE" 'module) (s module)
   "," (p "Function name: ") ",[" r "]}"))

;;
;; Functions that manipulate the code in useful(?) ways
;;
(defun eqc-mk-fancy-comment-h1 ()
  "Major comment fills line"
  (interactive)
  (eqc-mk-fancy-comment "%% -- " ?- 'fill)
  )

(defun eqc-mk-fancy-comment-h2 ()
  (interactive)
  (eqc-mk-fancy-comment "%% --- " ?- 3)
  )

(defun eqc-mk-fancy-comment-h3 ()
  (interactive)
  (eqc-mk-fancy-comment "%% ---- " ?- 0)
  )

(defun eqc-mk-fancy-comment-op ()
  (interactive)
  (eqc-mk-fancy-comment "%% --- Operation: " ?- 3)
  )

(defun eqc-mk-fancy-comment (startc fillc fillx)
  "Takes whatever is on line and wraps it into an Erlang comment
   optionallay filling the line up to 80 characters."
  (interactive)
  (end-of-line)
  (let ((epos (point)) spos)
    (beginning-of-line)
    (setq spos (point))
    (insert startc)
    (end-of-line)
    (insert " ")
    (cond
     ( (eq 'fill fillx)
         (insert (make-string (- (- 77 (length startc)) (- epos spos)) fillc)) )
     ( t
         (insert (make-string fillx fillc)) )
     )
    (forward-line)
    ))

(defun duplicate-function-clause ()
  "Using erlang-mark-clause and some copy, move cursor, paste, to
   create a copy of the current function clause. Useful when adding
   another pre/post/next_state..."
  (interactive)
  (erlang-mark-clause)
  (let (pos1 pos2 clause)
    (setq pos1 (region-beginning) pos2 (region-end))
    (setq clause (buffer-substring pos1 (- pos2 1)))
    (goto-char pos1)
    (insert clause)
    (insert ";\n")
    ))


(defun duplicate-function-clause-header ()
  "Using erlang-generate-new-clause to copy the function clause header"
  (interactive)
  ;; Since the erlang-beginning-of-clause, and thereby also
  ;; erlang-generate-new-clause, is broken (see comment in erlang.el
  ;; by andersl) this little trick makes the result less surprising...
  (if (and (bolp) (not (eolp)))
      (forward-char 1)
    nil)
  (let (tmp_var)
    (setq tmp_var erlang-new-clause-with-arguments)
    (setq erlang-new-clause-with-arguments t)
    (erlang-generate-new-clause)
    (setq erlang-new-clause-with-arguments tmp_var)
    ))

(defun change_statem_property_to_parallel ()
  ""
  (interactive)
  (erlang-mark-function)
  (let (pos1 pos2 fun)
    (setq pos1 (region-beginning) pos2 (region-end))
    (setq fun (buffer-substring pos1 (- pos2 1)))
    (setq fun (replace-regexp-in-string "\\(prop_[^(]*\\)" "\\1_parallel" fun))
    (setq fun (replace-regexp-in-string "run_commands(" "run_parallel_commands(" fun))
    (setq fun (replace-regexp-in-string "\\([^_]\\)commands(" "\\1parallel_commands(" fun))
    (setq fun (replace-regexp-in-string "H, S," "Seq, Par," fun))
    (erlang-end-of-function)
    (insert "\n")
    (insert fun)
    ))

;; Emacs vs. Xemacs stuff
(defun is-transient-mark-mode ()
  (if erlang-xemacs-p
      t
    transient-mark-mode
    ))

(defun is-mark-active ()
  (if erlang-xemacs-p
      (not (not (mark)))
    mark-active))

(defun eqc-show-abbreviations ()
  (with-output-to-temp-buffer "QuickCheck Abbreviations"
    (let ((help abbrev-help))
      (while help
        (princ (concat (car help) "\n"))
        (setq help (cdr help)))
    )
  ))

(defvar abbrev-help
  '("QuickCheck Abbreviations"
    "^^^^^^^^^^^^^^^^^^^^^^^^"
    "When using the QuickCheck Emacs mode a number of abbreviations are active."
    "Abbreviations (abbrevs) are triggered either by the QuickCheck bound \\C-q TAB, or"
    "by the default binding (usually \\C-c\\M-TAB)."
    ""
    "NOTE: For mysterious reasons abbrevs does not work on line 1 column 1..."
    ""
    "Below is a list of useful abbreviations defined by the QuickCheck mode:"
    ""
    "hdr          - Create an Erlang module header, and include eqc.hrl"
    "hdr-{module} - Erlang module header + eqc.hrl + eqc_{module}.hrl"
    ""
    "inc          - Include eqc.hrl"
    "inc-{module} - Include eqc_{module}.hrl"
    ""
    "op/fop/cop         - Add a new operation for eqc_statem/fsm/component, adds op_args() and op()"
    "iop/fiop/ciop      - Add a new operation interactively (yes-no for each callback)"
    "pre1/pre3          - Add <op>_pre/1 callback for eqc_statem(+component)/fsm"
    "                     <op> is found automagically by looking *below* point of insert"
    "                     (Note: statem and component have the same format, thus no 'cpre1')"
    "args/fargs         - Add <op>_args callback"
    "pre2/pre4          - Add <op>_pre/2 callback"
    "                   <op> is found automagically by looking *above* point of insert"
    "callers            - Add <op>_callers callback (only for eqc_component)"
    "callouts           - Add <op>_callouts callback (only for eqc_component)"
    "next/fnext         - Add <op>_next callback"
    "post/fpost         - Add <op>_post callback"
    "return             - Add <op>_return callback"
    "blocking/fblocking - Add <op>_blocking callback"
    "features/ffeatures - Add <op>_features callback"
    "adapt/fadapt       - Add <op>_adapt callback"
    "dynpre/fdynpre     - Add <op>_dynpre callback"
    ""
    "eqc_component        - Complete eqc_component specification"
    "eqc_fsm              - Complete eqc_fsm specification"
    "eqc_fsm_ungrouped    - Complete eqc_fsm specification (old ungrouped style)"
    "eqc_statem           - Complete eqc_statem specification"
    "eqc_statem_ungrouped - Complete eqc_statem specification (old ungrouped style)"
  ))

