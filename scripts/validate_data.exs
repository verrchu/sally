defmodule Script do
  require Logger

  def main([data_dir]) do
    Logger.info("Checking data consistency in #{data_dir}")

    schemas = Loader.load_schemas!(data_dir)
    codes = Loader.load_codes!(data_dir)
    ingredients = Loader.load_ingredients!(data_dir)
    recipes = Loader.load_recipes!(data_dir)
    steps = Loader.load_steps!(data_dir)

    :ok = Validator.validate_ingredients!(
      ingredients, schemas[:ingredient], codes
    )

    :ok = Validator.validate_known_codes!(codes)


    :ok = Validator.validate_recipes!(
      recipes, schemas[:recipe], ingredients, codes, steps
    )
  end
end

defmodule Validator do
  def validate_ingredients!(ingredients, schema, codes) do
    duplicate_ingredients = Util.duplicates(Map.keys(ingredients))
    unless Enum.empty?(duplicate_ingredients) do
      raise(
        """
        Duplicate ingedient definition
        Ingredients: #{inspect duplicate_ingredients}
        """
      )
    end

    Enum.each(ingredients, fn({name, ingredient}) ->
      ingredient_units = Map.get(ingredient, "units", [])
                         |> Enum.map(fn(unit) -> unit["name"] end)
      duplicate_units = Util.duplicates(ingredient_units)

      unless name == ingredient["name"] do
        raise(
          """
          Ingreient definition mismatched name
          Excpected: #{name}
          Actual: #{ingredient["name"]}
          """
        )
      end

      :ok = validate_schema!(ingredient, schema)
      :ok = validate_code!(ingredient["name"], codes)
    end)
  end

  def validate_recipes!(recipes, schema, ingredients, codes, steps) do
    duplicate_recipes = Util.duplicates(Map.keys(recipes))
    unless Enum.empty?(duplicate_recipes) do
      raise(
        """
        Duplicate recipes:
        Recipes: #{inspect duplicate_recipes}
        """
      )
    end

    Enum.each(recipes, fn({name, recipe}) ->
      unless name == recipe["name"] do
        raise(
          """
          Mismathed recipe name
          Expected: #{name}
          Actual: #{recipe["name"]}
          """
        )
      end

      :ok = validate_schema!(recipe, schema)
      :ok = validate_code!(recipe["name"], codes)
      :ok = validate_recipe_ingredients!(recipe, ingredients)
      :ok = validate_recipe_steps!(recipe, steps)
    end)
  end

  def validate_recipe_ingredients!(recipe, ingredients) do
    duplicate_recipe_ingredients = Util.duplicates(recipe["ingredients"])

    unless Enum.empty?(duplicate_recipe_ingredients) do
      raise(
        """
        Duplicate recipe ingredients
        Recipe: #{recipe["name"]}
        Ingredients: #{inspect duplicate_recipe_ingredients}
        """
      )
    end

    Enum.each(recipe["ingredients"], fn(recipe_ingredient) ->
      unless Map.has_key?(ingredients, recipe_ingredient["name"]) do
        raise(
          """
          Undefined ingredient
          Recipe: #{recipe["name"]}
          Ingredient: #{recipe_ingredient["name"]}
          """
        )
      end

      ingredient = ingredients[recipe_ingredient["name"]]
      ingredient_units = Map.get(ingredient, "units", [])
                         |> Enum.map(fn(unit) -> unit["name"] end)

      unless recipe_ingredient["quantity"] == "ANY" do
        unless recipe_ingredient["unit"] in ingredient_units do
          raise(
            """
            Incompatible ingedient unit
            Recipe: #{recipe["name"]}
            Ingredient: #{recipe_ingredient["name"]}
            Specified Unit: #{recipe_ingredient["unit"]}
            Defined Units: #{inspect ingredient_units}
            """
          )
        end
      end
    end)
  end

  def validate_recipe_steps!(reciep, steps) do
    :ok
  end

  def validate_code!(code, codes) do
    Enum.each(Data.langs(), fn(lang) ->
      codes = Map.get(codes, lang)

      unless Map.has_key?(codes, code) do
        raise(
          """
          Code not defined
          Code: #{code}
          Language: #{lang}
          """
        )
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
  @steps_file "steps.yaml"

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

  def load_steps!(data_dir) do
    localization_path = Path.join(data_dir, @localization_path)

    Enum.reduce(Data.langs(), %{}, fn(lang, acc) ->
      steps_path = Path.join([localization_path, lang, @steps_file])
      steps = Util.parse_yaml!(steps_path)

      Map.put(acc, lang, steps)
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

  def duplicates(data) when is_list(data) do
    data -- Enum.uniq(data)
  end
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

{:ok, _apps} = Application.ensure_all_started(:jason)
{:ok, _apps} = Application.ensure_all_started(:json_xema)
{:ok, _apps} = Application.ensure_all_started(:yaml_elixir)

Script.main(System.argv())
