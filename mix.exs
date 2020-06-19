defmodule Sally.MixProject do
  use Mix.Project

  def project do
    [
      app: :sally,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :yamerl]
    ]
  end

  defp deps do
    [
      {:redix, "~> 0.11"},
      {:yaml_elixir, "~> 2.4"},
      {:json_xema, "~> 0.4"},
      {:jason, "~> 1.2"}
    ]
  end
end
