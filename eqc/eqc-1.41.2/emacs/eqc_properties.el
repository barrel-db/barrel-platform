(defvar eqc-menu-properties
  (eqc-module-menu-prepare
   "Properties"
    '(["Full module header" tempo-template-eqc-header-eqc]
      ["Include eqc" tempo-template-eqc-include-eqc]
      ["Export all" tempo-template-eqc-export_all]
      ["--" 'ignore]
      ["Property" tempo-template-eqc-prop-general]
      )

    (eqc-menu-list-expand
     (append
       '(('mac "ALWAYS"    "prop-always"    ("Number of times to test" 'prop))
         ('mac "FORALL"    "prop-forall"    ("Value" "Generator" 'prop))
         ('mac "IMPLIES"   "prop-implies"   ("Precondition" 'prop))
         ('mac "ONCEONLY"  "prop-onceonly"  ('prop))
         ('mac "SOMETIMES" "prop-sometimes" ("Number of times to test" 'prop))
         ('mac "TIMEOUT"   "prop-timeout"   ("Timeout (ms)" 'prop))
         ('mac "TRAPEXIT"  "prop-trapexit"  ('prop) )
         ('mac "WHENFAIL"  "prop-whenfail"  ("Action" 'prop)))
       (cons ["--" 'ignore]
             eqc-prop-autogen-fun-menu-items))
      )
  ))

;; Property
(eqc-prop-def "general"
 '( (default-p "Property name (prop_NAME)" (erlang-get-module-from-file-name) 'prop) )
 '( "%% @doc Property testing ..." > n)
 '( "-spec prop_" (s prop) "() -> eqc:property()." > n)
 '( "prop_" (s prop) "(" (p "Property parameters (X, Y, ...): " params) ") ->" > n
    > (eqc-tempo-include tempo-template-eqc-macro-prop-forall) "." n )
 "property")

