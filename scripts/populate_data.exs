defmodule Script do
  require Logger

  def main([data_dir, langs]) do
    Logger.info("Populating data from #{data_dir}")

    {:ok, conn} = DataBase.connect()

    codes = DataLoader.load_codes!(data_dir, langs)

    :ok = DataBase.persist_codes!(conn, codes)

    steps = DataLoader.load_recipe_steps!(data_dir, langs)

    :ok = DataBase.persist_recipe_steps!(conn, steps)

    recipes = DataLoader.load_recipes!(data_dir)

    :ok = DataBase.persist_recipes!(conn, recipes)
  end

end

defmodule DataBase do
  require Logger

  def connect() do
    {:ok, [host: host, port: port]} = Confex.fetch_env(:sally, :db)

    Logger.info("Connecting to DB. Host: #{host}. Port: #{port}")

    {:ok, _conn} = Redix.start_link(host: host, port: port)
  end

  def persist_codes!(conn, codes) do
    Enum.each(codes, fn({lang, codes}) ->
      Enum.each(codes, fn({code, value}) ->
        key = "code:#{lang}:#{code}"

        Logger.debug("Persisting code. Key: #{key}. Value: #{value}")

        {:ok, "OK"} = Redix.command(conn, ["SET", key, value])
      end)
    end)
  end

  def persist_recipe_steps!(conn, steps) do
    Enum.each(steps, fn({lang, steps}) ->
      Enum.each(steps, fn({recipe, steps}) ->
        key = "recipe:#{recipe}:steps:#{lang}"

        Logger.debug("Persisting recipe steps. Key: #{key}. Recipe: #{recipe}. Lang: #{lang}")

        Enum.each(steps, fn(step) ->
          {:ok, _index} = Redix.command(conn, ["RPUSH", key, step])
        end)
      end)
    end)
  end

  def persist_recipes!(conn, recipes) do
    Enum.each(recipes, fn({recipe_name, recipe}) ->
      type_key = "recipe:#{recipe_name}:type"
      {:ok, "OK"} = Redix.command(conn, ["SET", type_key, Map.fetch!(recipe, "type")])

      ingredients_key = "recipe:#{recipe_name}:ingredients"

      Logger.debug(
        """
        Persisting recipe ingrdients.
        Recipe: #{recipe["name"]}.
        Key: #{ingredients_key}
        """
      )

      Enum.each(recipe["ingredients"], fn(ingredient) ->
        ingredient_name = Map.fetch!(ingredient, "name")
        {:ok, _index} = Redix.command(conn, ["RPUSH", ingredients_key, ingredient_name])

        ingredient_key = "recipe:#{recipe_name}:ingredient:#{ingredient_name}"

        Logger.debug(
          """
          Persisting recipe ingrdient.
          Recipe: #{recipe["name"]}.
          Ingredient: #{ingredient["name"]}.
          Key: #{ingredient_key}
          """
        )

        case Map.fetch!(ingredient, "quantity") do
          "ANY" ->
            {:ok, _index} = Redix.command(conn, [
              "HSET", ingredient_key, "quantity", "ANY"
            ])
          _ ->
            {:ok, "OK"} = Redix.command(conn, [
              "HMSET", ingredient_key,
              "quantity", Map.fetch!(ingredient, "quantity"),
              "unit", Map.fetch!(ingredient, "unit"),
              "min", Map.fetch!(ingredient, "min"),
              "max", Map.fetch!(ingredient, "max")
            ])
        end
      end)
    end)
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
