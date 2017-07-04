(defvar eqc-menu-eqc_mocking
  (eqc-module-menu-prepare
   "Mocking Erlang module(s)"
     '(["Full eqc_mocking module header" tempo-template-eqc-header-eqc_mocking]
       ["--" 'ignore]
       ["Include eqc_mocking" tempo-template-eqc-include-eqc_mocking]
       ["--" 'ignore]
       ["Complete API specification" tempo-template-eqc-fun-api_spec-erlang]
       ["New API module specification" tempo-template-eqc-fun-api_mod-erlang]
       ["New API function specification" tempo-template-eqc-fun-api_fun-erlang]
       ["New API function record" tempo-template-eqc-fun-api_rec_fun-erlang]
       )
     (eqc-menu-list-expand eqc-eqcmock-autogen-fun-menu-items)
     )
  )

(defvar eqc-menu-eqc_mocking_c
  (eqc-module-menu-prepare
   "Mocking C functions"
     '(["Full eqc_mocking_c module header" tempo-template-eqc-header-eqc_mocking_c]
       ["--" 'ignore]
       ["Include eqc_mocking_c" tempo-template-eqc-include-eqc_mocking_c]
       ["--" 'ignore]
       ["New API specification" tempo-template-eqc-fun-api_spec-c]
       ["New API module specification" tempo-template-eqc-fun-api_mod-c]
       ["New API function specification" tempo-template-eqc-fun-api_fun-c]
       ["New API function record" tempo-template-eqc-fun-api_rec_fun-c]
       ["New API argument record" tempo-template-eqc-fun-api_rec_arg-c]
       )
     (eqc-menu-list-expand eqc-eqcmockc-autogen-fun-menu-items)
     )
  )

(defun non-empty-action (var action)
  `(l (if (equal (tempo-lookup-named ',var) "") '() ',action) ))

(eqc-fun-def "api_spec-erlang"
 '( "%% @doc API specification for mocked components" > n)
 '( "-spec api_spec() -> #api_spec{}." > n)
 '( "api_spec() ->" > n
    (p "Name of mocked module [none]: " mmod t)
    "#api_spec{ language = erlang, mocking = eqc_mocking, modules = ["
    (non-empty-action 'mmod '(l (s mmod) "()"))
    "] }." > n n
    (non-empty-action 'mmod (eqc-tempo-include tempo-template-eqc-fun-api_mod-erlang))
  ))

(eqc-fun-pre-def "api_mod-erlang"
 '( (default-p "Name of mocked module" "mock" 'mmod) )
 '( "%% @doc API specification for mocked module" > n)
 '( "-spec " (s mmod) "() -> #api_module{}." > n)
 '( (s mmod) "() ->" > n
    (p "Name of mocked function [none]: " mfun t)
    "#api_module{ name = " (s mmod) ", fallback = undefined, functions = ["
    (non-empty-action 'mfun '(l (s mmod) "_" (s mfun) "()")) "] }." > n n
    (non-empty-action 'mfun (eqc-tempo-include tempo-template-eqc-fun-api_fun-erlang))
  ))

(eqc-fun-pre-def "api_fun-erlang"
 '( (default-p "Name of mocked function" "f" 'mfun) )
 '( "%% @doc API specification for mocked function" > n)
 '( "-spec " (non-empty-action 'mmod '(l (s mmod) "_")) (s mfun) "() -> #api_fun{}." > n)
 '( (non-empty-action 'mmod '(l (s mmod) "_")) (s mfun) "() ->" > n
    "#api_fun{ name = " (s mfun) ", arity = " ~ ", matched = all, fallback = false}." > n n
  ))

(eqc-fun-def "api_spec-c"
 '( "%% @doc API specification for mocked components" > n)
 '( "-spec api_spec() -> #api_spec{}." > n)
 '( "api_spec() ->" > n
    (p "Name of mocked module [none]: " mmod t)
    "#api_spec{ language = c, mocking = eqc_mocking, modules = ["
    (non-empty-action 'mmod '(l (s mmod) "()"))
    "] }." > n n
    (non-empty-action 'mmod (eqc-tempo-include tempo-template-eqc-fun-api_mod-c))
  ) "c-api")

(eqc-fun-pre-def "api_mod-c"
 '( (default-p "Name of mocked module" "mock" 'mmod) )
 '( "%% @doc API specification for mocked module" > n)
 '( "-spec " (s mmod) "() -> #api_module{}." > n)
 '( (s mmod) "() ->" > n
    (p "Name of mocked function [none]: " mfun t)
    "#api_module{ name = " (s mmod) ", fallback = undefined, functions = ["
    (non-empty-action 'mfun '(l (s mmod) "_" (s mfun) "()")) "] }." > n n
    (non-empty-action 'mfun (eqc-tempo-include tempo-template-eqc-fun-api_fun-c))
  ))

(eqc-fun-pre-def "api_fun-c"
 '( (default-p "Name of mocked function" "f" 'mfun) )
 '( "%% @doc API specification for mocked function" > n)
 '( "-spec " (non-empty-action 'mmod '(l (s mmod) "_")) (s mfun) "() -> #api_fun{}." > n)
 '( (non-empty-action 'mmod '(l (s mmod) "_")) (s mfun) "() ->" > n
    (p "Name of first argument [none]: " marg t)
    "#api_fun_c{ name = " (s mfun) ", ret = " ~ ", args = ["
    (non-empty-action 'marg '(l "#api_arg_c{ type = int, name = " (s marg) ", dir = in }"))
    "]}." > n n
  ))

(eqc-fun-def "api_rec_fun-erlang"
 '() '()
 '( (default-p "Name of mocked function" "f" 'marg)
    "#api_fun_c{ name = " (s marg) ", arity = " ~ " }"
  ) "erlang-fun")

(eqc-fun-def "api_rec_fun-c"
 '() '()
 '( (default-p "Name of mocked function" "f" 'marg)
    "#api_fun_c{ ret = " ~ ", name = " (s marg) ", args = {} }"
  ) "c-fun")

(eqc-fun-def "api_rec_arg-c"
 '() '()
 '( (default-p "Name of mocked argument" "a" 'marg)
    "#api_arg_c{ type = " ~ ", name = " (s marg) ", dir = in }"
  ) "c-arg")

