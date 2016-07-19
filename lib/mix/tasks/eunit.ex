defmodule Mix.Tasks.Eunit do
  use Mix.Task
  @recursive true

  @shortdoc "Compile and run eunit tests"

  def run(args) do
    options = parse_options(args)
    Mix.env :test
    post_config = eunit_post_config(Mix.Project.config)
    modify_project_config(post_config)
    ensure_compile
    post_config[:erlc_paths]
    |> update_test_dir(options)
    |> test_modules(options[:patterns])
    |> Enum.map(&module_name_from_path/1)
    |> Enum.drop_while(fn(m) -> tests_pass?(m, options[:eunit_options]) end)
  end

  defp parse_options(args) do
    {switches,
     argv,
     _errors} = OptionParser.parse(args,
                                  switches: [verbose: :boolean],
                                  switches: [only: :string],
                                  aliases: [v: :verbose])
    patterns = case argv do
                 [] -> ["*"]
                 p -> p
               end

    eunit_options = case switches[:verbose] do
                      true -> [:verbose]
                      _ -> []
                    end

    only = case String.valid?(switches[:only]) do
            true -> switches[:only]
            _ -> false
           end
    %{eunit_options: eunit_options, patterns: patterns, only: only}
  end

  defp eunit_post_config(existing_config) do
    [erlc_paths: existing_config[:erlc_paths] ++ ["test"],
     erlc_options: existing_config[:erlc_options] ++ [{:d, :TEST}]]
  end

  defp modify_project_config(post_config) do
    build_path = Mix.Project.build_path
    |> Path.split
    |> Enum.map(fn(p) -> filter_replace(p, "dev", "eunit") end)
    |> Path.join

    %{name: name, file: file} = Mix.Project.pop
    Mix.ProjectStack.post_config(Keyword.merge(post_config,
                                               [build_path: build_path]))
    Mix.Project.push name, file
  end

  defp filter_replace(x, x, r) do
    r
  end
  defp filter_replace(x, _y, _r) do
    x
  end

  defp ensure_compile do
    Mix.Task.reenable("compile")
    Enum.each(compilers, &Mix.Task.reenable/1)
  end

  defp compilers do
    Mix.Task.all_modules
    |> Enum.map(&Mix.Task.task_name/1)
    |> Enum.filter(fn(t) -> match?("compile." <> _, t) end)
  end

  defp test_modules(directories, patterns) do
    IO.inspect directories
    all_modules = erlang_source_files(directories, patterns)
    |> Enum.map(&module_name_from_path/1)
    |> Enum.uniq

    IO.inspect all_modules

    remove_test_duplicates(all_modules, all_modules, [])
  end

  defp erlang_source_files(directories, patterns) do
    Enum.map(patterns, fn(p) ->
               Mix.Utils.extract_files(directories, p <> ".erl")
             end)
    |> Enum.concat
    |> Enum.uniq
  end

  defp module_name_from_path(p) do
    Path.basename(p, ".erl") |> String.to_atom
  end

  defp remove_test_duplicates([], _all_module_names, accum) do
    accum
  end
  defp remove_test_duplicates([module | rest], all_module_names, accum) do
    module = Atom.to_string(module)
    if tests_module?(module) &&
      Enum.member?(all_module_names, without_test_suffix(module)) do
      remove_test_duplicates(rest, all_module_names, accum)
    else
      remove_test_duplicates(rest, all_module_names, [module | accum])
    end
  end

  def update_test_dir(erlc_paths, options) do
    case options[:only] do
      false -> erlc_paths
      only -> [only] 
    end
  end

  defp tests_module?(module_name) do
    String.match?(module_name, ~r/_tests$/)
  end

  defp without_test_suffix(module_name) do
    module_name
    |> String.replace(~r/_tests$/, "")
    |> String.to_atom
  end

  defp tests_pass?(module, eunit_options) do
    IO.puts("Running eunit tests in #{module}:")
    :ok == :eunit.test(module, eunit_options)
  end
end
