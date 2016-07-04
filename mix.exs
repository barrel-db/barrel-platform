defmodule Barrel.Mixfile do
  use Mix.Project

  def project do
    [app: :barrel,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     compilers: Mix.compilers,
     erlc_options: erlc_env_options ++ [:debug_info, parse_transform: :mochicow],
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
        :kernel,
        :stdlib,
        :crypto,
        :asn1,
        :public_key,
        :ssl,
        :os_mon,
        :inets,
        :p1_utils,
        :fast_yaml,
        :jsx,
        :ibrowse,
        :snappy,
        :ucol,
        :oauth,
        :hackney,
        :mochiweb,
        :mochicow,
        :cowlib,
        :ranch,
        :cowboy,
        :hooks,
        :exometer_core
      ]
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.0.2"},
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

  def erlc_env_options do
    case Mix.env do
      :prod -> [parse_transform: :lager_transform]
      :staging -> [parse_transform: :lager_transform]
      _ -> []
    end
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
