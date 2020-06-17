defmodule Script do
  require Logger

  def main([data_dir]) do
    Logger.info("Checking data consistency in #{data_dir}")

    schemas = Loader.load_schemas!(data_dir)
    codes = Loader.load_codes!(data_dir)
    ingredients = Loader.load_ingredients!(data_dir)

    :ok = Validator.validate_ingredients!(
      ingredients, schemas[:ingredient], codes
    )

    :ok = Validator.validate_known_codes!(codes)

    recipes = Loader.load_recipes!(data_dir)

    :ok = Validator.validate_recipes!(
      recipes, schemas[:recipe], ingredients, codes
    )
  end
end

defmodule Validator do
  def validate_ingredients!(ingredients, schema, codes) do
    # TODO: duplicate ingredients
    Enum.each(ingredients, fn({name, ingredient}) ->
      # TODO: duplicate units
      unless name == ingredient["name"] do
        Util.log_and_raise("ingredient definition for #{name} defines #{ingredient["name"]}")
      end

      :ok = validate_schema!(ingredient, schema)
      :ok = validate_code!(ingredient["name"], codes)
    end)
  end

  def validate_recipes!(recipes, schema, ingredients, codes) do
    # TODO: duplicate recipes
    Enum.each(recipes, fn({name, recipe}) ->
      unless name == recipe["name"] do
        Util.log_and_raise("recipe definition for #{name} defines #{recipe["name"]}")
      end

      :ok = validate_schema!(recipe, schema)
      :ok = validate_code!(recipe["name"], codes)
      :ok = validate_recipe_ingredients!(recipe, ingredients)
    end)
  end

  def validate_recipe_ingredients!(recipe, ingredients) do
    duplicate_ingredients = recipe["ingredients"] -- Enum.uniq(recipe["ingredients"])
    unless Enum.empty?(duplicate_ingredients) do
      Util.log_and_raise(
        "recipe #{recipe["name"]} contains duplicate ingredients #{inspect duplicate_ingredients}"
      )
    end

    Enum.each(recipe["ingredients"], fn(recipe_ingredient) ->
      unless Map.has_key?(ingredients, recipe_ingredient["name"]) do
        Util.log_and_raise(
          "recipe #{recipe["name"]} contains undefined ingredient #{recipe_ingredient["name"]}"
        )
      end
    end)
  end

  def validate_code!(code, codes) do
    Enum.each(Data.langs(), fn(lang) ->
      codes = Map.get(codes, lang)

      unless Map.has_key?(codes, code) do
        Util.log_and_raise("Code #{code} not defined for lang #{lang}")
      end
    end)
  end

  def validate_known_codes!(codes) do
    Enum.each(Data.known_codes(), fn(code) ->
      validate_code!(code, codes)
    end)
  end

  def validate_schema!(entity, schema) do
    JsonXema.validate!(schema, entity)
  end
end

defmodule Loader do
  @localization_path "localization"
  @ingredients_path "ingredients"
  @recipes_path "recipes"
  @schemas_path "schemas"

  @codes_file "codes.yaml"

  @schemas [
    recipe: "recipe.json",
    ingredient: "ingredient.json"
  ]

  def load_schemas!(data_dir) do
    schemas_path = Path.join(data_dir, @schemas_path)

    Enum.reduce(@schemas, %{}, fn({name, path}, acc) ->
      schema_path = Path.join(schemas_path, path)
      schema = schema_path |> Util.parse_json! |> JsonXema.new

      Map.put(acc, name, schema)
    end)
  end

  def load_codes!(data_dir) do
    localization_path = Path.join(data_dir, @localization_path)

    Enum.reduce(Data.langs(), %{}, fn(lang, acc) ->
      codes_path = Path.join([localization_path, lang, @codes_file])
      codes = Util.parse_yaml!(codes_path)

      Map.put(acc, lang, codes)
    end)
  end

  def load_ingredients!(data_dir) do
    ingredients_path = Path.join(data_dir, @ingredients_path)

    Enum.reduce(File.ls!(ingredients_path), %{}, fn(file, acc) ->
      ingredient_file = Path.join(ingredients_path, file)
      ingredient_name = Util.remove_file_ext(file) |> String.upcase
      ingredient = Util.parse_yaml!(ingredient_file)

      Map.put(acc, ingredient_name, ingredient)
    end)
  end

  def load_recipes!(data_dir) do
    recipes_path = Path.join(data_dir, @recipes_path)

    Enum.reduce(File.ls!(recipes_path), %{}, fn(file, acc) ->
      recipe_file = Path.join(recipes_path, file)
      recipe_name = Util.remove_file_ext(file) |> String.upcase
      recipe = Util.parse_yaml!(recipe_file)

      Map.put(acc, recipe_name, recipe)
    end)
  end
end

defmodule Util do
  require Logger

  def parse_yaml!(path), do: parse_file!(path, :yaml)
  def parse_json!(path), do: parse_file!(path, :json)

  defp parse_file!(path, :yaml) do
    path |> File.read! |> YamlElixir.read_from_string!
  end

  defp parse_file!(path, :json) do
    path |> File.read! |> Jason.decode!
  end

  def remove_file_ext(file), do: hd(String.split(file, "."))

  def log_and_raise(err_msg), do: err_msg |> Logger.error |> raise
end

defmodule Data do
  @recipe_types ["BREAKFAST"]
  @ingredient_units ["GRAM"]
  @ingresient_characteristics [
    "CALORIES", "PROTEINS", "FATS", "CARBOHYDRATES"
  ]

  def known_codes(), do: (
    @recipe_types ++ @ingredient_units ++ @ingresient_characteristics
  )

  def langs(), do: ["ru", "en"]
end

{:ok, _apps} = Application.ensure_all_started(:json_xema)
{:ok, _apps} = Application.ensure_all_started(:yaml_elixir)

Script.main(System.argv())
