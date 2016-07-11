defmodule Barrel.Mixfile do
  use Mix.Project

  def project do
    [app: :barrel,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     compilers: Mix.compilers,
     erlc_options: [:debug_info, parse_transform: :lager_transform, parse_transform: :mochicow],
     deps: deps,
     package: package,
     description: description
   ]
  end

  def application do
    [
      mod: {:barrel_app, []},
      applications:
      [
        :lager_logger,
        :kernel,
        :stdlib,
        :crypto,
        :sasl,
        :asn1,
        :public_key,
        :ssl,
        :os_mon,
        :inets,
        :p1_utils,
        :gproc,
        :ibrowse,
        :jsx,
        :ucol,
        :oauth,
        :fast_yaml,
        :ranch,
        :cowboy,
        :mochiweb,
        :mochicow,
        :hooks,
        :econfig,
        :hackney,
        :exometer_core,
        :snappy
      ]
    ]
  end

  defp deps do
    [
      {:lager_logger, "~> 1.0"},
      {:exrm, "~> 0.18.1"},
      {:barrel_nifs, path: "support/barrel_release_plugin"},
      {:metrics, "~> 1.0.1", manager: :rebar, override: true},
      {:econfig, "~> 0.7.3", manager: :rebar, override: true},
      {:ranch, "~> 1.2.1", manager: :rebar, override: true},
      {:hooks,  "~> 1.2", manager: :rebar, override: true},
      {:mochicow, "~> 0.6.4", manager: :rebar, override: true},
      {:snappy, "~> 1.1.1", manager: :rebar, override: true},
      {:ucol, "~> 2.0", manager: :rebar, override: true},
      {:folsom, "~> 0.8.3", manager: :rebar, override: true},
      {:ssl_verify_fun, "~> 1.1.0", manager: :rebar, override: true},
      {:edown, "~> 0.7.0", manager: :rebar, override: true},
      {:parse_trans, "~> 2.9.0", manager: :rebar, override: true},
      {:certifi, "~> 0.4.0", manager: :rebar, override: true},
      {:gproc, "~> 0.5.0"},
      {:mochicow, "~> 0.6.4", manager: :rebar, override: true},      
      {:hackney, "~> 1.6.0", manager: :rebar, override: true},
      {:p1_utils, "~> 1.0.4", manager: :rebar, override: true},
      {:oauth, "~> 1.6", hex: :barrel_oauth, manager: :rebar, override: true},
      {:ibrowse, "~> 4.3.1", hex: :barrel_ibrowse, manager: :rebar, override: true},
      {:mochiweb, "~> 2.15.0", manager: :rebar, override: true},
      {:cowlib, "~> 1.0.2", manager: :rebar, override: true},
      {:goldrush, "~> 0.1.7", manager: :rebar, override: true},
      {:cowboy, "~> 1.0.4", manager: :rebar, override: true},
      {:lager, "~> 3.0.2", manager: :rebar, override: true},
      {:exometer_core, "~> 1.4.0", manager: :rebar, override: true},      
      {:jsx, "~> 2.8.0"},
      {:stache, "~> 0.2.1"},
      {:econfig, "~> 0.7.3"},
      {:exometer_core, "~> 1.4.0"},
      {:fast_yaml, "~> 1.0.5", manager: :rebar, override: true}
    ]
  end

  defp description do
    """
    Barrel is a distributed database for the modern world. It is a document-oriented database targeting data locality & P2P.
    """
  end

  defp package do
    [
     name: :barrel,
     files: ["lib", "priv", "mix.exs", "README*", "LICENSE*", "Makefile", "NOTICE", "rebar.config",
     "rebar3", "utilities", "support", "src", "rel", "include", "config", "c_src", "attic"],
     maintainers: ["Benoit Chesneau"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/barrel-db/barrel-platform",
              "Docs" => "https://docs.barrel-db.org/docs"}]
  end
end
