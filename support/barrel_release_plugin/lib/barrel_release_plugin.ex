defmodule ReleaseManager.Plugin.BarrelRelease do
  use ReleaseManager.Plugin
  
  def before_release(_) do
    {result, _errcode} = System.cmd("make", ["-C", "c_src/barrel_js"], into: IO.stream(:stdio, :line))
  end

  def after_release(%Config{name: app, version: version} = _config) do
    {result, _errcode} = System.cmd("make", ["-C", "c_src/barrel_js", "clean"], into: IO.stream(:stdio, :line))
    System.cmd("escript", ["support/build_js.escript"], into: IO.stream(:stdio, :line))
    BarrelRelease.SetupConfig.update_config("config/sys.ini", "rel/#{app}/releases/#{version}/sys.ini")
    BarrelRelease.SetupConfig.update_config("config/sys.config", "rel/#{app}/releases/#{version}/sys.config")
    BarrelRelease.SetupConfig.update_config("config/vm.args", "rel/#{app}/releases/#{version}/vm.args")
  end

  def after_package(_args) do
  end

  def after_cleanup(_args) do
    
  end
end
