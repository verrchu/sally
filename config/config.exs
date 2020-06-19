import Config

config :logger, :console,
  level: :debug,
  format: "\n$date $time $metadata[$level] $levelpad$message\n",
  metadata: [:pid]

config :sally, :db,
  host: {:system, :string, "SALLY_DB_HOST"},
  port: {:system, :integer, "SALLY_DB_PORT"}

import_config "#{Mix.env()}.exs"
