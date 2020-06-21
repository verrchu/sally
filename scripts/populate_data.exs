defmodule Script do
  require Logger

  def main([data_dir, langs]) do
    Logger.info("Populating data from #{data_dir}")

    {:ok, conn} = DataBase.connect()
    :ok = DataBase.flush(conn)

    codes = DataLoader.load_codes!(data_dir, langs)
    :ok = DataBase.persist_codes!(conn, codes)

    ingredients = DataLoader.load_ingredients!(data_dir)
    :ok = DataBase.persist_ingredients!(conn, ingredients)

    recipes = DataLoader.load_recipes!(data_dir)
    :ok = DataBase.persist_recipes!(conn, recipes)

    steps = DataLoader.load_recipe_steps!(data_dir, langs)
    :ok = DataBase.persist_recipe_steps!(conn, steps)

    # measures = DataLoader.load_measures!(data_dir, langs)
    # :ok = DataBase.persist_measures!(conn, measures)
  end
end

defmodule DataBase do
  require Logger

  def connect() do
    {:ok, [host: host, port: port]} = Confex.fetch_env(:sally, :db)

    Logger.info("Connecting to DB. Host: #{host}. Port: #{port}")

    {:ok, _conn} = Redix.start_link(host: host, port: port)
  end

  def flush(conn) do
    {:ok, "OK"} = Redix.command(conn, ["FLUSHALL"])

    :ok
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

        {:ok, _index} = Redix.command(
          conn, List.flatten(["SADD", key, steps])
        )
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
        Persisting recipe ingredients.
        Recipe: #{recipe["name"]}.
        Key: #{ingredients_key}
        """
      )

      Enum.each(recipe["ingredients"], fn(ingredient) ->
        ingredient_name = Map.fetch!(ingredient, "name")
        {:ok, _index} = Redix.command(
          conn, ["SADD", ingredients_key, ingredient_name]
        )

        ingredient_key = "recipe:#{recipe_name}:ingredient:#{ingredient_name}"

        Logger.debug(
          """
          Persisting recipe ingredient.
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

  def persist_ingredients!(conn, ingredients) do
    Enum.each(ingredients, fn({ingredient_name, ingredient}) ->
      units_key = "ingredient:#{ingredient_name}:units"
      Logger.debug(
        """
        Persisting ingredient.
        Ingredient: #{ingredient["name"]}.
        Key: #{units_key}
        """
      )

      Enum.each(Map.fetch!(ingredient, "units"), fn(unit) ->
        unit_name = Map.fetch!(unit, "name")
        {:ok, _index} = Redix.command(conn, ["SADD", units_key, unit_name])

        unit_characteristics = Map.fetch!(unit, "characteristics")

        unit_key = "ingredient:#{ingredient_name}:unit:#{unit_name}"

        Logger.debug(
          """
          Persisting ingredient characteristics.
          Ingredient: #{ingredient["name"]}.
          Unit: #{unit_name}
          Key: #{unit_key}
          """
        )

        {:ok, "OK"} = Redix.command(conn, [
          "HMSET", unit_key,
          "quantity", Map.fetch!(unit, "quantity"),
          "calories", Map.fetch!(unit_characteristics, "calories"),
          "carbohydrates", Map.fetch!(unit_characteristics, "carbohydrates"),
          "fats", Map.fetch!(unit_characteristics, "fats"),
          "proteins", Map.fetch!(unit_characteristics, "proteins")
        ])
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
