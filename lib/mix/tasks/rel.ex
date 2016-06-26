defmodule Mix.Tasks.Rel do
  use Mix.Task

  @shortdoc "Barrel release manager"

  def run(_args) do
    File.mkdir("rel")
    File.touch("rel/relx.config")
    case Mix.env do
        :prod -> config(:rel)
        :staging -> config(:staging)
        _ -> config(:devrel)
    end
    System.cmd("mix", ["release"], into: IO.stream(:stdio, :line))
  end

  def config(:devrel) do
    BarrelRelease.SetupConfig.update_config("config/relx.config", "rel/relx.config", %{dev_mode: "true", include_erts: "false", rel_vsn: "0.1.0"})
  end

  def config(:staging) do
    BarrelRelease.SetupConfig.update_config("config/relx.config", "rel/relx.config", %{dev_mode: "true", include_erts: "true", rel_vsn: "0.1.0"})
  end

  def config(:rel) do
    BarrelRelease.SetupConfig.update_config("config/relx.config", "rel/relx.config", %{dev_mode: "false", include_erts: "true", rel_vsn: "0.1.0"})
  end
end