import Config

config :logger, :console,
  level: :debug,
  format: "\n$date $time $metadata[$level] $levelpad$message\n",
  metadata: [:pid]

import_config "#{Mix.env()}.exs"
