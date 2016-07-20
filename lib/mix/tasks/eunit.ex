defmodule Mix.Tasks.Eunit do
  use Mix.Task

  @shortdoc "Run the project's EUnit test suite"

  def run(args) do
    {opts, args, rem_opts} = OptionParser.parse(args, strict: [verbose: :boolean], aliases: [v: :verbose])
    new_args = args ++ Barrel.Util.filter_opts(rem_opts)
    Mix.env :etest

    compile_opts = [{:d,:TEST}|Mix.Project.config[:erlc_options]]
    System.put_env "ERL_COMPILER_OPTIONS", format_compile_opts(compile_opts)

    Mix.Task.run "compile", new_args

    ebin_test = Path.join([Mix.Project.app_path, "test_beams"])
    Barrel.Util.compile_files(Path.wildcard("**/*_tests.erl"), ebin_test)

    options = if Keyword.get(opts, :verbose, false), do: [:verbose], else: []
    :eunit.test {:application, Mix.Project.config[:app]}, options
  end

  defp format_compile_opts(opts) do
    :io_lib.format("~p", [opts]) |> List.to_string
  end
end