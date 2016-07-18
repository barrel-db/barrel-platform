defmodule Mix.Tasks.Eunit do
  use Mix.Task

  @shortdoc "Run a project's eunit tests"
  @recursive true

  def run(args) do
    {opts, _, _} = OptionParser.parse(args,
      strict: [verbose: :boolean],
      aliases: [v: :verbose]
    )

    unless System.get_env("MIX_ENV") || Mix.env == :dev do
      Mix.raise "mix eunit is running on environment #{Mix.env}. If you are running tests" <>
                " along another task, please set MIX_ENV explicitly"
    end

    Mix.Task.run "loadpaths", args

    project = Mix.Project.config

    Mix.shell.print_app
    Mix.Task.run "app.start", args

    case Application.load(:eunit) do
      :ok -> :ok
      { :error, { :already_loaded, :eunit } } -> :ok
    end

    options = if Keyword.get(opts, :verbose, false), do: [:verbose], else: []
    :eunit.test {:application, project[:app]}, options
  end
end