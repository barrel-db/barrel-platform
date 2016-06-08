defmodule ReleaseManager.Plugin.BarrelRelease do
  use ReleaseManager.Plugin
  
  def before_release(_) do
    {result, _errcode} = System.cmd("make", ["-C", "c_src/barrel_js"])
    info result
  end

  def after_release(%Config{name: app, version: version, relx_config: relx_config} = config) do
    {result, _errcode} = System.cmd("make", ["-C", "c_src/barrel_js", "clean"])
    info result
    BarrelRelease.SetupConfig.update_config("rel/#{app}/releases/#{version}/sys.config")
    BarrelRelease.SetupConfig.update_config("rel/#{app}/releases/#{version}/vm.args")
  end

  def after_package(_args) do
  end

  def after_cleanup(_args) do
    
  end
end