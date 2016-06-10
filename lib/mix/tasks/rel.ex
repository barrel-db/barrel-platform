defmodule Mix.Tasks.Rel do
use Mix.Task

  @shortdoc "Barrel development release"

  def run(_args) do
    BarrelRelease.SetupConfig.update_config("config/relx.config", "rel/relx.config", %{dev_mode: "false", include_erts: "true"})
    System.put_env("MIX_ENV", "PROD")
    System.cmd("mix", ["release"], into: IO.stream(:stdio, :line))
  end
end