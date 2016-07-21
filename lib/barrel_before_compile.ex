defmodule BarrelBeforeCompile do  
  def __before_compile__(env) do
    Mix.Shell.IO.info("=====> Compiling support.")
    System.cmd("make", ["-C", "c_src/barrel_js"], stderr_to_stdout: true)
    System.cmd("escript", ["support/build_js.escript"], stderr_to_stdout: true)
    Mix.Shell.IO.info("=====> Finished compiling support.")
  end
end