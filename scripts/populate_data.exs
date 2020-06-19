defmodule Script do
  require Logger

  def main([data_dir, langs]) do
    Logger.info("Populating data from #{data_dir}")

    DataLoader.load_recipes!(data_dir)
  end
end

[data_dir, langs] = System.argv()
langs = String.split(langs, ",")

[{DataLoader, _}] = Code.require_file(
  Path.join([File.cwd!(), "scripts", "util", "data_loader.exs"])
)

Script.main([data_dir, langs])
