defmodule Script do
  require Logger

  def main([data_dir, langs]) do
    Logger.info("Populating data from #{data_dir}")

    {:ok, conn} = connect()

    codes = DataLoader.load_codes!(data_dir, langs)

    :ok = DataBase.persist_codes!(conn, codes)
  end

  defp connect() do
    {:ok, [host: host, port: port]} = Confex.fetch_env(:sally, :db)

    Logger.info("Connecting to DB. Host: #{host}. Port: #{port}")

    {:ok, _conn} = Redix.start_link(host: host, port: port)
  end
end

defmodule DataBase do
  require Logger

  def persist_codes!(conn, codes) do
    Enum.each(codes, fn({lang, codes}) ->
      Enum.each(codes, fn({code, value}) ->
        key = "code:#{lang}:#{code}"

        Logger.debug("Persisting code. Key: #{key}. Value: #{value}")

        :ok = set_key(conn, key, value)
      end)
    end)
  end

  defp set_key(conn, key, value) do
    {:ok, "OK"} = Redix.command(conn, ["SET", key, value])
    :ok
  end
end

{:ok, _apps} = Application.ensure_all_started(:confex)
{:ok, _apps} = Application.ensure_all_started(:redix)

[data_dir, langs] = System.argv()
langs = String.split(langs, ",")

[{DataLoader, _}] = Code.require_file(
  Path.join([File.cwd!(), "scripts", "util", "data_loader.exs"])
)

Script.main([data_dir, langs])
