(defun eqc-module-menu-prepare (name static funs)
  (cons name (append static (cons ["--" 'ignore] funs)))
)

(defun eqc-menu-list-expand (lst)
  (mapcar 'eqc-menu-macro-expand lst)
)

(defun eqc-menu-macro-expand (def)
  (cond
   ( (not (listp def))
     def)
   ( (equal (nth 0 def) ''mac)
     (eqc-menu-macro-expand-inner "macro" "Macro" "?" def) )

   ( (equal (nth 0 def) ''fun)
     (eqc-menu-macro-expand-inner "fun" "Function" "" def) )
   ))

(defun eqc-menu-macro-expand-inner (type typename prefix def)
  (let ((name (nth 1 def)) (fname (nth 2 def)) (args (nth 3 def)) res)
    (tempo-define-template
     (concat "eqc-" type "-" fname)
     (build-def-str name args prefix)
     nil)
    (setq res (vector (concat typename " " name)
                      (intern (concat "tempo-template-eqc-" type "-" fname))))
    (if (not (null (nth 4 def)))
        (setq res (vconcat res (vector :help (nth 4 def)))))
    res)
)

(defun build-def-str (name args prefix)
  (cons (concat prefix (replace-regexp-in-string "/[0-9]" "" name) "(")
          (nconc (build-macro-args-str args nil) (list ")")))
  )

(defun build-macro-args-str (args comma)
  (cond
   ( (null args)
     nil)
   ( (equal comma t)
     (cons "," (build-macro-args-str args nil)))
   ( t
     (let ((rest (build-macro-args-str (cdr args) t)))
       (cond
        ( (equal (car args) ''prop)
          (nconc (list '> 'n '> 'r>) rest))
        ( (equal (car args) ''gen)
          (nconc (list '> 'r>) rest))
        ( t
          (cons (list 'p (concat (car args) ": ") (intern (car args))) rest)
          ))))
   ))

;; Split overly long menus
(defun eqc-split-long-menu (menu)
  "Splitting long menus"
  (if (listp menu)
      (let ((split_menu (mapcar 'eqc-split-long-menu menu)))
        (eqc-split-long-menu-inner split_menu))
    menu
    )
)

(defun eqc-split-long-menu-inner (menu)
  (cond
   ( (<= (length menu) eqc-max-menu-length) ;; Shorter than max-length
     menu
     )
   ( t
     (let ((menu2 (nthcdr eqc-max-menu-length menu))
           (menu1 (nbutlast menu (- (length menu) eqc-max-menu-length))))
       (append menu1 (list (cons "More..." (eqc-split-long-menu-inner menu2))))
       )
     )
   ))


(defun eqc-find-op-name (&optional dir_opt)
  (interactive)
  (let ((dir (if (or (null dir_opt) (equal dir_opt 'up)) 'up 'down))
        (opname nil) op1 op2)
    (setq op1 (eqc-find-op-name2 dir))
    (setq op2 (eqc-find-op-name2 (if (equal dir 'up) 'down 'up)))
    (cond ((eq op1 op2) (setq opname op1))
          ((null op1) (setq opname op2))
          ((null op2) (setq opname op1))
          (t         (setq opname op1))) ;;preferred direction in op1
    (if (null opname) (setq opname "op"))
    opname ))

(defun eqc-chomp-postfix (fun_name)
  (condition-case nil
    (setq fun_name
          (replace-regexp-in-string
            (concat "_\\(pre\\|post\\|args\\|next\\|callouts\\|callers\\|return\\|blocking"
                    "\\|dynamicpre\\|features\\|command\\|adapt\\)$") "" fun_name))
    (error nil))
  fun_name)

(defun eqc-trim-str (str)
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" str)))

(defun eqc-find-op-name2 (dir)
  "Tries to find the name of the current operation searching for another op-function"
  "in the direction indicated"
  (interactive)
  (let ((opname "op") pstart pend str)
    (save-excursion
      (condition-case nil
        (progn
          (when (equal dir 'down) (erlang-end-of-function))
          (setq pend (point))
          (erlang-beginning-of-function)
          (setq pstart (point))
          (re-search-forward "(" pend)
          (backward-char 1)
          (setq pend (point))
          (setq str (buffer-substring-no-properties pstart pend))
          (setq str (replace-regexp-in-string "[ \t]*-spec[ \t]*" "" str))
          (setq opname (eqc-chomp-postfix str)))
        (error (setq opname "op"))
        ) )
    opname)
  )

(defun eqc-find-fun (fun_name)
  (let ((found nil))
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
      (when (re-search-forward (concat "^" fun_name "(") (point-max))
          (setq found (point)))
      (error nil)
    )
  )
  found
  ))

;; Note: changes point
(defun eqc-goto-fun (fun_name)
  (let (pos)
    (setq pos (eqc-find-fun fun_name))
    (unless (null pos) (goto-char pos))
    (not (null pos))
  ))

(defun eqc-get-args (fun_name)
  (let ((fun_args nil))
    (save-excursion
      (when (eqc-goto-fun fun_name)
        (progn
          (erlang-beginning-of-clause)
          (setq fun_args (erlang-get-function-arguments))))
    )
    fun_args
  ))

(defun eqc-find-op-args (op_name &optional opt_default)
  (let ((op_args nil) (default (if (null opt_default) "_Args" opt_default)))
    (setq op_args (eqc-get-args op_name))
    (if (null op_args)
      (setq op_args default)
      (setq op_args (concat "[" op_args "]")))
    op_args
  ))

(defun eqc-is-in (what)
  (let ((ans nil) p1 p2 p3)
  (save-excursion
  (condition-case nil
    (progn
      (setq p1 (point))
      (re-search-backward (concat what "("))
      (setq p2 (point))
      (forward-sexp 2)
      (setq p3 (point))
      (when (and (> p1 p2) (< p1 p3)) (setq ans p2))
      )
    (error nil)
  ))
  ans
))

(defun eqc-goto-callback (fun_name)
  (let ((ans nil) pos)
    (setq pos (eqc-find-fun (concat fun_name "_callouts")))
    (when (null pos) (setq pos (eqc-find-fun (concat fun_name "_next"))))
    (when (null pos) (setq pos (eqc-find-fun (concat fun_name "_pre"))))
    (unless (null pos) (progn
                         (setq eqc-saved-location (point)) ;; use internal mark
                         (push-mark nil t)                 ;; and normal mark
                         (goto-char pos)
                         (erlang-beginning-of-clause)))
    (not (null pos))
  ))

(defun eqc-get-first-arg (pos)
  (save-excursion
  (condition-case nil
    (let (start)
      (goto-char pos)
      (forward-sexp)
      (setq start (+ (point) 1))
      (goto-char start)
      (forward-sexp)
      (erlang-buffer-substring start (point)))
    (error nil)
  )))

(defun eqc-get-second-arg (pos)
  (save-excursion
  (condition-case nil
    (let (start)
      (goto-char pos)
      (forward-sexp)
      (goto-char (+ (point) 1))
      (forward-sexp)
      (setq start (+ (point) 1))
      (goto-char start)
      (forward-sexp)
      (eqc-trim-str (erlang-buffer-substring start (point))))
    (error nil)
  )))

(defvar eqc-current-op-name nil)
(defvar eqc-current-params nil)
(defvar eqc-saved-location nil)

(defun eqc-set-current-vars (opname params)
  (setq eqc-current-op-name opname)
  (setq eqc-current-params params) )

(defun eqc-current-function ()
  (save-excursion
    (if (not (eobp)) (forward-char 1))
      (and (erlang-beginning-of-clause) (erlang-get-function-name)))
  )

(defun eqc-goto-selfcall-place ()
  "We should be in a ?APPLY thus in a _callouts..., get the name for this one"
  "Then move forward as long as the function name matches..."
  (let ((cur_fun (eqc-current-function)) (again 2) tmp_name p)
    (if (null cur_fun) (progn (erlang-end-of-function) (insert "\n"))
      (progn
        (while (> again 1)
          (setq p (point))
          (erlang-end-of-function 2) (erlang-beginning-of-function)
          (when (equal p (point)) (setq again 1))
          (setq tmp_name (erlang-get-function-name))
          (unless (equal (string-match (eqc-chomp-postfix cur_fun) tmp_name) 0) (setq again 0)))
        (when (equal again 0) (erlang-beginning-of-function))
        (erlang-end-of-function)
        (insert "\n")
      ))
    ))

(defun eqc-create-selfcall (fun_name args)
  (let ((input (read-string (concat "Create " fun_name "_callouts/_next/_pre? [NO/c/p/n]: ") nil nil "no")))
    (unless (or (equal input "no") (equal input "NO"))
      (progn
        (eqc-set-current-vars fun_name args)
        (condition-case nil (progn
          (eqc-goto-selfcall-place)
          (cond ((equal input "c") (tempo-template-eqc-add-callouts))
                ((equal input "n") (tempo-template-eqc-add-next))
                ((equal input "p") (tempo-template-eqc-add-pre_2))
                (t nil)))
          (error nil))
        (eqc-set-current-vars nil nil)))
  ))

(defun eqc-go-back ()
  (interactive)
  (unless (null eqc-saved-location)
    (progn (goto-char eqc-saved-location) (setq eqc-saved-location nil))))

(defun eqc-goto-or-create ()
  (interactive)
  (let ((is-in nil) (fun nil) (args nil) (goto nil))
    (setq is-in (eqc-is-in "?APPLY"))
    (unless (null is-in) (setq fun (eqc-get-first-arg is-in)))
    (unless (null is-in) (setq args (eqc-get-second-arg is-in)))
    (when (or (null is-in) (null fun)) (error "Navigation failed: use only inside ?APPLY"))
    (setq eqc-saved-location (point))
    (unless (null fun) (setq goto (eqc-goto-callback fun)))
    (when (null goto) (eqc-create-selfcall fun args))
  ))

