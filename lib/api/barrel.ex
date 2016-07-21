defmodule Barrel do
  @moduledoc """
  This module ports the functions and API calls in barrel.erl
  """

  @before_compile BarrelBeforeCompile

  defdelegate [start_listener(atom, list),stop_listener(atom), start_console(list), stop_console(), info()], to: :barrel
end

