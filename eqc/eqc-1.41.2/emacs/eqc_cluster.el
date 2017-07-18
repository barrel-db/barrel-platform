;; QuickCheck mode definitions for eqc_cluster

(defvar eqc-menu-eqc_cluster
  (eqc-module-menu-prepare
   "Cluster specs"
   '(["Complete eqc_cluster spec" tempo-template-eqc-module-eqc_cluster]
     ["--" 'ignore]
     ["Full eqc_cluster module header" tempo-template-eqc-header-eqc_cluster]
     ["--" 'ignore]
     ["Include eqc_cluster" tempo-template-eqc-include-eqc_cluster]
     ["Default Cluster Property" tempo-template-eqc-prop-eqc_cluster]
     ["--" 'ignore]
     ["callback api_spec" tempo-template-eqc-fun-api_spec-cluster]
     ["callback components" tempo-template-eqc-fun-components]
     ["callback weight" tempo-template-eqc-fun-weight-cluster]
     )
   (eqc-menu-list-expand eqc-cluster-autogen-fun-menu-items))
  )

;; Complete eqc_component spec skeleton
(tempo-define-template "eqc-module-eqc_cluster"
 '(
   (eqc-tempo-include tempo-template-eqc-header-eqc_cluster) n
   (eqc-tempo-include tempo-template-eqc-fun-components) n
   (eqc-tempo-include (eqc-comment-template tempo-template-eqc-fun-weight-cluster)) n
   (eqc-tempo-include tempo-template-eqc-fun-api_spec-cluster) n

   (tempo-save-named 'module "?MODULE")
   (tempo-save-named 'prop (erlang-get-module-from-file-name))
   (eqc-tempo-include tempo-template-eqc-prop-eqc_cluster)
   )
 "eqc_cluster"
 )

;; Cluster property
(eqc-prop-def "eqc_cluster"
 '( (default-p "Property name (prop_NAME)" (erlang-get-module-from-file-name) 'prop) )
 '( "%% @doc Default generated property" > n )
 '( "-spec prop_" (s prop) "() -> eqc:property()." > n )
 '( "prop_" (s prop) "() ->" > n
    "?SETUP(" > n
    (eqc-indent-string 2) "fun() ->" n
    "%% Setup mocking, etc." > n
    "eqc_mocking:start_mocking(api_spec())," > n
    "%% Return the teardwown function" > n
    "fun() -> ok end" > n
    "end," > n
    (default-p "Module for commands" "?MODULE" 'module)
    (eqc-indent-string 1) "?FORALL(Cmds, commands(" (s module) ")," n
    (eqc-indent-string 1) "begin" n
    "{H, S, Res} = run_commands(Cmds)," > n
    "pretty_commands(?MODULE, Cmds, {H, S, Res}," > n
    (eqc-indent-string 3) "measure(length, length(Cmds),"  n
    (eqc-indent-string 4) "aggregate(command_names(Cmds)," n
    (eqc-indent-string 4 (length "aggregate(")) (if (is-region-active) 'r> "Res == ok")
    ")))" n
    "end))." > n)
  "component_property")

(eqc-fun-def "weight-cluster"
 '( "%% @doc Weights defining the distribution of components." > n)
 '( "-spec weight(Component :: atom()) -> integer()." > n)
 '( "weight(_Component) -> 1." > n ))

(eqc-fun-def "api_spec-cluster"
 '( "%% @doc API specification for mocked modules interfacing the cluster" > n)
 '( "-spec api_spec() -> #api_spec{}." > n)
 '( "api_spec() -> eqc_cluster:api_spec(?MODULE)." > n ))

(eqc-fun-def "components"
 '( "%% @doc Lists the components that make up the cluster." > n)
 '( "-spec components() -> [atom()]." > n)
 '( "components() -> [" ~ "]." > n ))

