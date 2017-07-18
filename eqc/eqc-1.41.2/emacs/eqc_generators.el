(defvar eqc-menu-generators
  (eqc-module-menu-prepare
   "Generators"
     '(["Sized generator" tempo-template-eqc-sized_generator]
       )
      ;; MACROS
     (eqc-menu-list-expand
      (append
       '(('mac "LET"           "gen-let"           ("Bound variable" "Generator for variable" 'gen))
         ('mac "SIZED"         "gen-sized"         ("Size variable" 'gen))
         ('mac "SUCHTHAT"      "gen-suchthat"      ("Bound variable" "Generator for variable" "Condition"))
         ('mac "SUCHTHATMAYBE" "gen-suchthatmaybe" ("Bound variable" "Generator for variable" "Condition"))
         ('mac "SHRINK"        "gen-shrink"        ('gen "List of generators"))
         ('mac "LETSHRINK"     "gen-letshrink"     ("Bound variable" "Generator (should be a list)" 'gen))
         ('mac "LAZY"          "gen-lazy"          ('gen)))
       (cons ["--" 'ignore] eqc-gen-autogen-fun-menu-items))
      )
     ))

;; Sized generator
(tempo-define-template
 "eqc-sized_generator"
 '(& (p "generator name: " gen_name) "() ->" > n
     "?SIZED(Size, " (s gen_name) "(Size))." > n n
     (s gen_name) "(0) ->" > n
     > p ";" n
     (s gen_name) "(Size) ->" > n
     > p "." n)
 "sized_gen")

