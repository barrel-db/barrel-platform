defmodule BarrelRelease.SetupConfig do
  def update_config(path) do
    vars = get_vars("config/vars.config")
    new_config = Stache.eval_file(path, vars)
    file = File.open! path, [:write]
    IO.inspect new_config
    IO.binwrite file, new_config
  end

  def get_vars(path) do
    {:ok, terms} = :file.consult(path)
    terms
    |> Enum.map(fn {var, value} -> {var, to_string(value)} end )
    |> Enum.into(%{})
  end
end