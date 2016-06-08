defmodule ReleaseManager.Plugin.BarrelRelease do
  use ReleaseManager.Plugin
  
  def before_release(_) do
    {result, _errcode} = System.cmd("make", ["-C", "c_src/barrel_js"])
    info result
  end

  def after_release(%Config{name: app, version: version, relx_config: relx_config} = config) do
    {result, _errcode} = System.cmd("make", ["-C", "c_src/barrel_js", "clean"])
    info result
    IO.puts "after release"
    #../../config/sys.config
    IO.inspect System.cmd("pwd", [])
    #IO.inspect File.read! "rel/#{app}/releases/#{version}/sys.config"
    BarrelRelease.SetupConfig.update_config("rel/#{app}/releases/#{version}/sys.config")
    BarrelRelease.SetupConfig.update_config("rel/#{app}/releases/#{version}/vm.args")
    IO.inspect File.read! "rel/#{app}/releases/#{version}/sys.config"
    #IO.inspect File.read! "rel/#{app}/releases/#{version}/sys.config"
    #IO.inspect ReleaseManager.Config.package
  end

  def after_package(%Config{name: app, version: version, relx_config: relx_config} = config) do
    IO.inspect File.read! "rel/#{app}/releases/#{version}/sys.config"
  end

  def after_cleanup(_args) do
    
  end
end