# BarrelElixir

Demo app with barrel as a dependency

  1. Add `barrel` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:barrel, "~> 0.1.0"}]
    end
    ```

  2. Ensure `barrel` is started before your application:

    ```elixir
    def application do
      [applications: [:barrel]]
    end
    ```

