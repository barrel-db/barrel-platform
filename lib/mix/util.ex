defmodule Barrel.Util do
  def filter_opts(opts) do
    Enum.reduce(opts, [], fn
      {name, nil}, acc -> [name|acc]
      {name, val}, acc -> [name, val | acc]
    end)
  end

  def compile_files(files, dir) do
    File.mkdir_p!(dir)

    status = Enum.reduce(files, :ok, fn path, status ->
      case :compile.file(String.to_char_list(path),
        [{:outdir, String.to_char_list(dir)}, :report]) do
        {:ok, _} -> IO.puts "Compiled #{path}"; status
        :error -> :error
      end
    end)
    if status == :error, do:  Mix.raise "Encountered compilation errors"

    Code.prepend_path(dir)
  end
end