defmodule Mix.Tasks.Devrel do
use Mix.Task

  @shortdoc "Barrel development release"

  def run(_args) do
    BarrelRelease.SetupConfig.update_config("config/relx.config", "rel/relx.config", %{dev_mode: "true", include_erts: "false"})
    System.cmd("mix", ["release"], into: IO.stream(:stdio, :line))
  end
end