defmodule Script do
  require Logger

  def main([data_dir]) do
    Logger.info("Populating data from #{data_dir}")

    DataLoader.load_recipes!(data_dir) |> IO.inspect
  end
end

[{DataLoader, _}] = Code.require_file(
  Path.join([File.cwd!(), "scripts", "util", "data_loader.exs"])
)

Script.main(System.argv())
