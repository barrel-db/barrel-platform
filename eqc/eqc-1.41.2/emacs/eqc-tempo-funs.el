;; define a property
(defun eqc-prop-def (name prefix doc spec code &optional name2)
  "Meta function for defining a property with(out) docspec"
  (tempo-define-template
   (concat "eqc-prop-" name)
   `(& (eqc-tempo-include (quote ,prefix))
       (eqc-tempo-include (if (not eqc-include-docs) nil (quote ,doc)))
       (eqc-tempo-include (if (not eqc-include-specs) nil (quote ,spec)))
       (eqc-tempo-include (quote ,code)))
   (concat "prop-" (if (null name2) name name2)))
)

;; define a function
(defun eqc-fun-def (name doc spec code &optional name2)
  "Meta function for defining functions with(out) docspec"
  (tempo-define-template
   (concat "eqc-fun-" name)
   `(& (eqc-tempo-include (if (not eqc-include-docs) nil (quote ,doc)))
       (eqc-tempo-include (if (not eqc-include-specs) nil (quote ,spec)))
       (eqc-tempo-include (quote ,code)))
   (concat "fun-" (if (null name2) name name2)))
)

;; define a function with prefix
(defun eqc-fun-pre-def (name prefix doc spec code &optional name2)
  "Meta function for defining functions with(out) docspec"
  (tempo-define-template
   (concat "eqc-fun-" name)
   `(& (eqc-tempo-include (quote ,prefix))
       (eqc-tempo-include (if (not eqc-include-docs) nil (quote ,doc)))
       (eqc-tempo-include (if (not eqc-include-specs) nil (quote ,spec)))
       (eqc-tempo-include (quote ,code)))
   (concat "fun-" (if (null name2) name name2)))
)

;; Insert a tempo template and comment out the inserted code
(defun eqc-insert-comment-template (template)
  (save-excursion
    (tempo-insert-template template tempo-insert-region)
    (if (and (and (integerp tempo-start-pos) (integerp tempo-end-pos))
             (> tempo-end-pos tempo-start-pos))
        (comment-region tempo-start-pos tempo-end-pos)
      nil))
)

(defun add-comments (list)
  (let ((hd (car list)) (tl (cdr list)))
    (if tl
        (if (eq 'n hd)
            (cons hd (cons "%% " (add-comments tl)))
          (cons hd (add-comments tl)))
      list))
)

(defun add-spec-comments (list)
  (let ((hd (car list)) (tl (cdr list)))
    (if tl
        (if (and (stringp hd) (string= (substring hd 0 5) "-spec"))
            (cons "%% " (cons hd (add-comments tl))) ;; This is correct!
          (cons hd (add-spec-comments tl)))
      list))
)

(defun eqc-comment-template (template)
  (let ((expr0 (car (last template))) (expr nil)
        (doc0 (car (last (butlast template)))) (doc nil)
        newtemplate)
    (if (and (eq 'eqc-tempo-include (nth 0 expr0))
             (eq 'quote (nth 0 (nth 1 expr0))))
        (setq expr (nth 1 (nth 1 expr0))))
    (if (and (eq 'eqc-tempo-include (nth 0 doc0))
             (eq 'if (nth 0 (nth 1 doc0))))
        (setq doc (nth 1 (nth 3 (nth 1 doc0)))))
    (setq expr (cons "%% " (add-comments expr)))
    (setq doc (add-spec-comments doc))
    (setq expr0 (list 'eqc-tempo-include (list 'quote expr)))
    (setq doc0 (list 'eqc-tempo-include
                     (append (butlast (nth 1 doc0))
                             (list (list 'quote doc)))))

    (setq newtemplate (append (butlast template 2) (list doc0 expr0)))
    newtemplate
    )
)

;; Help functions
(defun default-insert (var default)
  (if (equal (tempo-lookup-named var) "")
      default
    (tempo-lookup-named var))
  )

(defun is-region-active ()
  "Checks if there is a region selected"
  (interactive)
  (let
      (pos1 pos2)
    (if (and (is-transient-mark-mode) (is-mark-active))
        (setq pos1 (region-beginning) pos2 (region-end))
      (setq pos1 0 pos2 0))
    (not (equal pos1 pos2))))

;; Setting the cursor position
(defvar tempo-initial-pos nil
  "Initial position in template after expansion")

(defvar tempo-start-pos nil)
(defvar tempo-end-pos   nil)

(defadvice tempo-insert( around tempo-insert-pos2 act )
  "Define initial position."
  (if (eq element '~)
      (setq tempo-initial-pos (point-marker))
    ad-do-it)
  (setq tempo-end-pos (point))
)

(defadvice tempo-insert-template( around tempo-insert-template-pos2 act )
  "Set initial position when defined"
  (setq tempo-initial-pos nil)
  (setq tempo-start-pos (point))
  ad-do-it
  (if tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

;; Function that makes it possible to includ a tempo macro inside
;; another tempo macro
(defun eqc-tempo-include (&rest args)
  "Include a template inside another template.
Technically, this function returns the `tempo' attribute`(l ...)' which
can contain other `tempo' attributes. "
  (let ((res '())
        entry)
    (while args
      (setq entry (car args))
      (while entry
        (setq res (cons (car entry) res))
        (setq entry (cdr entry)))
      (setq args (cdr args)))
    (cons 'l (nreverse res))))

;; Helper functions for interactive inclusion of new command/operation
(defun op_do (vname tname)
  (if (equal (tempo-lookup-named vname) "y")
    `(l (eqc-tempo-include ,tname) n)
    ""
  ))

(defun op_ask (opname template)
  (unless (equal (tempo-lookup-named 'op_ans) "q")
    (progn
      (tempo-save-named 'op_ans nil)
      `(l (p ,(concat "Include <op>_" (concat opname "? [y/N/q]: ")) op_ans t)
          (op_do 'op_ans ',template))
      )
  ))

(defun ask-record (rec_var &optional default)
  (tempo-save-named 'tmp_rec_var default)
  `(l (p "Use a state record? [Y/n]: " use_rec t)
      (unless (equal (tempo-lookup-named 'use_rec) "n")
        `(l (p "Record name? [state]: " tmp_rec_var t)
            (when (equal (tempo-lookup-named 'tmp_rec_var) "") (tempo-save-named 'tmp_rec_var "state"))
            "-record(" (s tmp_rec_var) ",{" p "})." n n
            ))
      (tempo-save-named ',rec_var (tempo-lookup-named 'tmp_rec_var))
   ))

;; Tempo prompt with default option
(defun default-p (prompt def_val var)
  `(l (p ,(concat prompt (concat " (default " (concat def_val "): "))) ,var t)
      (when (equal (tempo-lookup-named ',var) "") (tempo-save-named ',var ,def_val)))
  )

(defun eqc-apply-to-args (str fun)
  (if (equal "" str) str
    (let (tokens var (atoms '()))
      (setq tokens (split-string str ","))
      (while tokens
       (setq var (eqc-trim-str (car tokens)))
       (unless (equal "" var)
        (setq atoms (cons (funcall fun var) atoms)))
       (setq tokens (cdr tokens)))
      (mapconcat 'identity (reverse atoms) ", ")
    )
  ))

(defun eqc-atomify (str)
  (eqc-apply-to-args str 'eqc-mk-atom))

(defun eqc-underscore (str)
  (eqc-apply-to-args str 'eqc-mk-underscore))

(defun eqc-mk-atom (str)
  (concat "'" str "'"))

(defun eqc-mk-underscore (str)
  (if (or (equal "" str) (equal (string-to-char str) ?_)) str
    (concat "_" str)))

;; Tempo prompt for operation arguments
;; reads 'op_name
;; writes 'args, 'gens *and* 'params
(defun op_args-p ()
  `(l ,(default-p  "Name of operation" "op" 'op_name)
      (op_args-p2 (tempo-lookup-named 'op_name))
      (tempo-save-named 'args (tempo-lookup-named 'tmp_args))
      (tempo-save-named 'gens (eqc-atomify (tempo-lookup-named 'tmp_args)))
      (tempo-save-named 'params (concat "[" (eqc-underscore (tempo-lookup-named 'tmp_args)) "]"))
      (mk-op-comment 'op_name) > n
      )
  )

(defun op_args-p2 (op)
  `(p ,(concat "Arguments for `" op "` (default <no arguments>): ") tmp_args t)
  )

;; Setting up Args for add-on callback
(defun set-params (&optional prompt)
  (if (null eqc-current-params)
    (tempo-save-named 'params  (eqc-find-op-args (tempo-lookup-named 'op_name) nil))
    (tempo-save-named 'params eqc-current-params))
  (when (null prompt) (default-p "Operation arguments" "_Args" 'params))
  (when (null (tempo-lookup-named 'params)) (tempo-save-named 'params "_Args"))
  )

;; Make an appropriate comment for an operation
(defun mk-comment-h1 (str)
  (concat "%% -- " str " " (make-string (- 78 (+ (length str) 7)) ?-)))

(defun mk-op-comment (op_name)
  (let ((str (tempo-lookup-named 'op_name)))
  (concat "%% --- Operation: " str " ---")))

;; Comment in steps of erlang-indent-level
(defun eqc-indent-string (level &optional extra)
  (or extra (setq extra 0))
  (or erlang-indent-level (setq erlang-indent-level 2))
  (make-string (+ extra (* level erlang-indent-level)) ?\s))

