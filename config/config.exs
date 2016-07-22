use Mix.Config
config :lager, :error_logger_redirect, false
config :lager, :error_logger_whitelist, [Logger.ErrorHandler]
config :lager, :crash_log, false
config :lager, :handlers, [{LagerLogger, [level: :debug]}]