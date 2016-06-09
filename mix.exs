defmodule Barrel.Mixfile do
  use Mix.Project

  def project do
    [app: :barrel,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     compilers: Mix.compilers,
     erlc_options: [:debug_info, parse_transform: :lager_transform],
     deps: deps
   ]
  end

  def application do
    [
      mod: {:barrel_app, []},
      applications:
      [
        :lager,
        :kernel,
        :stdlib,
        :crypto,
        :sasl,
        :asn1,
        :public_key,
        :ssl,
        :os_mon,
        :inets,
        :gproc,
        :ibrowse,
        :snappy,
        :jsx,
        :ucol,
        :oauth,
        :mochiweb,
        :ranch,
        :cowboy,
        :hooks,
        :econfig,
        :hackney,
        :exometer_core
      ]
    ]
  end

  defp deps do
    [
      {:lager, "3.0.2"},
      {:exrm, "~> 0.18.1"},
      {:barrel_nifs, path: "utilities/barrel_release_plugin"},
      {:hooks,  "~> 1.1.1"},
      {:snappy, "~> 1.1"},
      {:ucol, "~> 2.0"},
      {:gproc, "~> 0.5.0"},
      {:cowboy, "~> 1.0"},
      {:mochiweb, "~> 2.12.2"},
      {:oauth, "~> 1.6", hex: :barrel_oauth},
      {:ibrowse, "~> 4.2", hex: :barrel_ibrowse},
      {:hackney, "~> 1.6"},
      {:jsx, "~> 2.8.0"},
      {:econfig, "~> 0.7.2"},
      {:exometer_core, "~> 1.0"}
    ]
  end
end
