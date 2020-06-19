defmodule Script do
  require Logger

  def main([data_dir, langs]) do
    Logger.info("Validating data in #{data_dir}")

    schemas = DataLoader.load_schemas!(data_dir)
    codes = DataLoader.load_codes!(data_dir, langs)
    ingredients = DataLoader.load_ingredients!(data_dir)
    recipes = DataLoader.load_recipes!(data_dir)
    steps = DataLoader.load_recipe_steps!(data_dir, langs)

    :ok = Validator.validate_ingredients!(
      ingredients, schemas[:ingredient], codes, langs
    )

    :ok = Validator.validate_known_codes!(codes, langs)

    :ok = Validator.validate_recipes!(
      recipes, schemas[:recipe], ingredients, codes, steps, langs
    )
  end
end

defmodule Validator do
  def validate_ingredients!(ingredients, schema, codes, langs) do
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
      unless Enum.empty?(duplicate_units) do
        raise(
          """
          Duplicate ingredient units
          Ingredient: #{ingredient["name"]}
          Units: #{inspect duplicate_units}
          """
        )
      end

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
      :ok = validate_code!(ingredient["name"], codes, langs)
    end)
  end

  def validate_recipes!(recipes, schema, ingredients, codes, steps, langs) do
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
      :ok = validate_code!(recipe["name"], codes, langs)
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

  def validate_recipe_steps!(recipe, steps) do
    Enum.each(steps, fn({lang, steps}) ->
      unless Map.has_key?(steps, recipe["name"]) do
        raise(
          """
          Recipe steps not defined
          Recipe: #{recipe["name"]}
          Lang: #{lang}
          """
        )
      end

      recipe_steps = steps[recipe["name"]]

      unless is_list(recipe_steps) && Enum.all?(recipe_steps, &is_binary/1) do
        raise(
          """
          Recipe steps malformed
          Recipe: #{recipe["name"]}
          Lang: #{lang}
          """
        )
      end
    end)
  end

  def validate_code!(code, codes, langs) do
    Enum.each(langs, fn(lang) ->
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

  def validate_known_codes!(codes, langs) do
    recipe_types = ["BREAKFAST"]
    ingredient_units = ["GRAM"]
    ingresient_characteristics = [
      "CALORIES", "PROTEINS", "FATS", "CARBOHYDRATES"
    ]

    known_codes = (
      recipe_types ++ ingredient_units ++ ingresient_characteristics
    )

    Enum.each(known_codes, fn(code) ->
      validate_code!(code, codes, langs)
    end)
  end

  def validate_schema!(entity, schema) do
    JsonXema.validate!(schema, entity)
  end
end

defmodule Util do
  def duplicates(data) when is_list(data) do
    data -- Enum.uniq(data)
  end
end

{:ok, _apps} = Application.ensure_all_started(:jason)
{:ok, _apps} = Application.ensure_all_started(:json_xema)
{:ok, _apps} = Application.ensure_all_started(:yaml_elixir)

[data_dir, langs] = System.argv()
langs = String.split(langs, ",")

[{DataLoader, _}] = Code.require_file(
  Path.join([File.cwd!(), "scripts", "util", "data_loader.exs"])
)

Script.main([data_dir, langs])
