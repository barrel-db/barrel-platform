defmodule Mix.Tasks.Ct do
  use Mix.Task

  @shortdoc "Run the project's Common Test suite"

  def run(args) do
    {opts, args, rem_opts} = OptionParser.parse(args, strict: [log_dir: :string])
    new_args = args ++ filter_opts(rem_opts)

    Mix.env :test

    Mix.Task.run "compile", new_args

    ebin_dir = Path.join([Mix.Project.app_path, "test_beams"])
    compile_files(Path.wildcard("test/**/*_SUITE.erl"), ebin_dir)

    logdir = Keyword.get(opts, :log_dir, "ctest/logs")
    File.mkdir_p!(logdir)

    :ct.run_test [
      {:dir, String.to_char_list(ebin_dir)},
      {:logdir, String.to_char_list(logdir)},
      {:auto_compile, false}
    ]
  end

  def filter_opts(opts) do
    Enum.reduce(opts, [], fn
      {name, nil}, acc -> [name|acc]
      {name, val}, acc -> [name, val | acc]
    end)
  end

  def compile_files(files, dir) do
    File.mkdir_p!(dir)
    status = Enum.reduce(files, :ok, fn path, status ->
      case :compile.file(String.to_char_list(path), [{:outdir, String.to_char_list(dir)}, :report]) do
          {:ok, _} -> IO.puts "Compiled #{path}"; status
          :error -> :error
        end
    end)
    if status == :error, do:  Mix.raise "Encountered compilation errors"

    Code.prepend_path(dir)
  end
end