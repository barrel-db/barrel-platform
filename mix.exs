defmodule Barrel.Mixfile do
  use Mix.Project

  def project do
    [app: :barrel,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     compilers: [:support] ++ Mix.compilers,
     erlc_options: [:debug_info, parse_transform: :lager_transform, parse_transform: :mochicow],
     deps: deps,
     erlc_paths: ["lib", "src"],
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
      {:metrics, "~> 1.0.1"},
      {:econfig, "~> 0.7.3"},
      {:ranch, "~> 1.2.1"},
      {:hooks,  "~> 1.2"},
      {:mochicow, "~> 0.6.4"},
      {:snappy, "~> 1.1.1"},
      {:ucol, "~> 2.0"},
      {:folsom, "~> 0.8.3"},
      {:ssl_verify_fun, "~> 1.1.0"},
      {:edown, "~> 0.7.0"},
      {:parse_trans, "~> 2.9.0"},
      {:certifi, "~> 0.4.0"},
      {:gproc, "~> 0.5.0"},
      {:mochicow, "~> 0.6.4"},
      {:p1_utils, "~> 1.0.4"},
      {:oauth, "~> 1.6", hex: :barrel_oauth},
      {:ibrowse, "~> 4.3.1", hex: :barrel_ibrowse},
      {:mochiweb, "~> 2.15.0"},
      {:cowlib, "~> 1.0.2"},
      {:goldrush, "~> 0.1.7"},
      {:cowboy, "~> 1.0.4"},
      {:lager, "~> 3.0.2"},
      {:exometer_core, "~> 1.4.0"},
      {:hackney, "~> 1.6.0"},      
      {:jsx, "~> 2.8.0"},
      {:stache, "~> 0.2.1"},
      {:econfig, "~> 0.7.3"},
      {:exometer_core, "~> 1.4.0"},
      {:fast_yaml, "~> 1.0.5"}
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


defmodule Mix.Tasks.Compile.Support do
  use Mix.Task
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.Support"

  def run(args) do
    Mix.Shell.IO.info("=====> Compiling support")
    System.cmd("make", ["-C", "c_src/barrel_js"], into: IO.stream(:stdio, :line))
    System.cmd("escript", ["support/build_js.escript"], into: IO.stream(:stdio, :line))
    Mix.Shell.IO.info("=====> Finished compiling support.")
    {:ok, :done}
  end

  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  def clean, do: Erlang.clean(manifest())
end