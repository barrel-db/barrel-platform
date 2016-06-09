defmodule BarrelRelease.SetupConfig do
  def update_config(source, destination) do
    vars = get_vars("config/vars.config")
    new_config = Stache.eval_file(source, vars)
    file = File.open! destination, [:write]
    IO.binwrite file, new_config
  end

  def get_vars(path) do
    {:ok, terms} = :file.consult(path)
    terms
    |> Enum.map(fn {var, value} -> {var, to_string(value)} end )
    |> Enum.into(%{})
  end
end