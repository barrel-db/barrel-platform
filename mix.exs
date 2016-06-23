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
        :barrel_ex_logger,
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
      {:barrel_ex_logger, git: "https://github.com/barrel-db/barrel_ex_logger.git"},
      {:exrm, "~> 0.18.1"},
      {:barrel_nifs, path: "support/barrel_release_plugin"},
      {:hooks,  "~> 1.1.1"},
      {:mochicow, "~> 0.6.4"},
      {:snappy, "~> 1.1"},
      {:ucol, "~> 2.0"},
      {:gproc, "~> 0.5.0"},
      {:oauth, "~> 1.6", hex: :barrel_oauth},
      {:ibrowse, "~> 4.3.1", hex: :barrel_ibrowse},
      {:hackney, "~> 1.6"},
      {:jsx, "~> 2.8.0"},
      {:stache, "~> 0.2.1"},
      {:econfig, "~> 0.7.3"},
      {:exometer_core, "~> 1.4.0"},
      {:fast_yaml, "~> 1.0.3"}
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
