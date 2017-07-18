(defvar eqc-menu-eqc_temporal
  (eqc-module-menu-prepare "Temporal specifications"
     '(["Full eqc_temporal module header" tempo-template-eqc-header-eqc_temporal]
       ["--" 'ignore]
       ["Include eqc_temporal" tempo-template-eqc-include-eqc_temporal]
       )
     (eqc-menu-list-expand eqc-eqctemp-autogen-fun-menu-items)
     )
  )
