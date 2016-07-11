defmodule BarrelConfig do
  @moduledoc """
  This module provides access to `barrel_config` via `BarrelConfig`
  """
  defdelegate [start(), env(), get_env(atom), set_env(atom, value), process_env(list), config_file(), init_config(), read_file(string)], to: :barrel_config
end