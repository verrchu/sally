defmodule SchemaLoader do
  @behaviour Xema.Loader

  def init() do
    :ok = Application.put_env(:xema, :loader, __MODULE__)
  end

  @spec fetch(URI.t()) :: {:ok, any} | {:error, any}
  def fetch(uri) do
    Application.fetch_env!(:sally, :data_dir)
    |> Path.join("schemas")
    |> Path.join(uri.path)
    |> File.read!()
    |> Jason.decode()
  end
end
