defmodule ReleaseManager.Plugin.BarrelRelease do
  use ReleaseManager.Plugin
  
  def before_release(_) do
    System.cmd("make", ["-C", "c_src/barrel_js"], into: IO.stream(:stdio, :line))
  end

  def after_release(%Config{name: app, version: version} = _config) do
    System.cmd("make", ["-C", "c_src/barrel_js", "clean"], into: IO.stream(:stdio, :line))
    System.cmd("escript", ["support/build_js.escript"], into: IO.stream(:stdio, :line))
    File.mkdir("rel/#{app}/etc")
    File.touch("rel/#{app}/etc/barrel.yml")
    BarrelRelease.SetupConfig.update_config("config/barrel.yml", "rel/#{app}/etc/barrel.yml")
    BarrelRelease.SetupConfig.update_config("config/sys.config", "rel/#{app}/releases/#{version}/sys.config")
    BarrelRelease.SetupConfig.update_config("config/vm.args", "rel/#{app}/releases/#{version}/vm.args")
  end

  def after_package(_args) do
  end

  def after_cleanup(_args) do
    
  end
end
