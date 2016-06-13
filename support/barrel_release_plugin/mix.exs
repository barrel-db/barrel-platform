defmodule BarrelReleasePlugin.Mixfile do
  use Mix.Project

  def project do
    [app: :barrel_nifs,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
      {:exrm, "~> 0.18.1"},
      {:stache, "~> 0.2.1"}
    ]
  end
end
