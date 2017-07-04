(defvar eqc-menu-eqc_c
  (eqc-module-menu-prepare
   "C code specifications"
     '(["Full eqc_c module header" tempo-template-eqc-header-eqc_c]
       ["--" 'ignore]
       ["Include eqc_c" tempo-template-eqc-include-eqc_c]
       )
     (eqc-menu-list-expand eqc-eqcc-autogen-fun-menu-items)
     )
  )
