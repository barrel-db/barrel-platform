defmodule ReleaseManager.Plugin.BarrelRelease do
  use ReleaseManager.Plugin
  
  def before_release(_) do
    System.cmd("make", ["-C", "c_src/barrel_js"], into: IO.stream(:stdio, :line))
  end

  def after_release(%Config{name: app, version: version, env: env} = _config) do
    System.cmd("make", ["-C", "c_src/barrel_js", "clean"], into: IO.stream(:stdio, :line))
    System.cmd("escript", ["support/build_js.escript"], into: IO.stream(:stdio, :line))
    File.mkdir("rel/#{app}/etc")
    File.mkdir("rel/#{app}/etc/barrel.d")
    File.touch("rel/#{app}/etc/barrel.yml")
    File.touch("rel/#{app}/etc/barrel.d/lager.yml")
    setup(app, version, env)
  end

  def after_package(_args) do
  end

  def after_cleanup(_args) do

  end

  def setup(app, version, :dev) do
    BarrelRelease.SetupConfig.update_config("config/barrel.yml", "rel/#{app}/etc/barrel.yml", "config/vars.config")
    BarrelRelease.SetupConfig.update_config("config/lager.yml", "rel/#{app}/etc/barrel.d/lager.yml", "config/vars.config")
    BarrelRelease.SetupConfig.update_config("config/sys.config", "rel/#{app}/releases/#{version}/sys.config", "config/vars.config")
    BarrelRelease.SetupConfig.update_config("config/vm.args", "rel/#{app}/releases/#{version}/vm.args", "config/vars.config")
  end

  def setup(app, version, :prod) do
    BarrelRelease.SetupConfig.update_config("config/barrel.yml", "rel/#{app}/etc/barrel.yml", "config/prod.config")
    BarrelRelease.SetupConfig.update_config("config/lager.yml", "rel/#{app}/etc/barrel.d/lager.yml", "config/prod.config")
    BarrelRelease.SetupConfig.update_config("config/sys.config", "rel/#{app}/releases/#{version}/sys.config", "config/prod.config")
    BarrelRelease.SetupConfig.update_config("config/vm.args", "rel/#{app}/releases/#{version}/vm.args", "config/prod.config")
  end
end
