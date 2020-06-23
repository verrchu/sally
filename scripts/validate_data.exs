defmodule Script do
  require Logger

  def main([data_dir, langs]) do
    Logger.info("Validating data in #{data_dir}")

    schemas = DataLoader.load_schemas!(data_dir)

    codes = DataLoader.load_codes!(data_dir, langs)
    steps = DataLoader.load_recipe_steps!(data_dir, langs)

    :ok = Validator.validate_known_codes!(codes, langs)

    measures = DataLoader.load_measures!(data_dir, langs)
    :ok = Validator.validate_measures!(measures, schemas)

    ingredients = DataLoader.load_ingredients!(data_dir)
    :ok = Validator.validate_ingredients!(ingredients, schemas, codes, langs)

    recipes = DataLoader.load_recipes!(data_dir)
    :ok = Validator.validate_recipes!(
      recipes, schemas, ingredients, codes, steps, langs
    )
  end
end

defmodule Validator do
  require Logger

  @measures ["GRAM"]

  def validate_measures!(measures, schemas) do
    schema = Map.fetch!(schemas, :measure)

    Enum.each(measures, fn({lang, measures}) ->
      diff = @measures -- Map.keys(measures)

      unless Enum.empty?(diff) do
        raise(
          """
          Measures not defined
          Measures: #{inspect diff}
          """
        )
      end

      Enum.each(measures, fn({name, measure}) ->
        Logger.debug("Validating measure #{name}. Lang: #{lang}")

        :ok = validate_schema!(measure, schema)
      end)
    end)
  end

  def validate_ingredients!(ingredients, schemas, codes, langs) do
    Logger.info("Validating ingredients")

    Enum.each(ingredients, fn({ingredient_type, ingredients}) ->
      Enum.each(ingredients, fn({ingredient_name, ingredient}) ->
        Logger.debug(
          """
          Validating ingedient
          Ingredient: #{ingredient_name}
          Ingredient type: #{ingredient_type}

          """
        )

        schema_name = String.to_atom("#{ingredient_type}_ingredient")
        schema = Map.fetch!(schemas, schema_name)

        :ok = validate_code!(ingredient_name, codes, langs)
        :ok = validate_schema!(ingredient, schema)
      end)
    end)
  end

  def validate_recipes!(recipes, schemas, ingredients, codes, steps, langs) do
    Logger.info("Validating recipes")

    schema = Map.fetch!(schemas, :recipe)

    Enum.each(recipes, fn({recipe_name, recipe}) ->
      Logger.debug("Validating recipe #{recipe_name}")

      recipe_ingredients = Map.fetch!(recipe, "ingredients")

      :ok = validate_schema!(recipe, schema)
      :ok = validate_code!(recipe_name, codes, langs)
      :ok = validate_recipe_steps!(recipe_name, steps)
      :ok = validate_recipe_ingredients!(
        recipe_name, recipe_ingredients, ingredients
      )
    end)
  end

  def validate_recipe_ingredients!(recipe_name, recipe_ingredients, ingredients) do
    Enum.each(recipe_ingredients, fn({ingredient_type, recipe_ingredients}) ->
      Enum.each(recipe_ingredients, fn({ingredient_name, recipe_ingredient}) ->
        Logger.debug(
          """
          Validating recipe ingredient
          Recipe: #{recipe_name}
          Ingredient: #{ingredient_name}
          Ingredient type: #{ingredient_type}
          """
        )

        ingredients = Map.fetch!(ingredients, ingredient_type)

        unless Map.has_key?(ingredients, ingredient_name) do
          raise(
            """
            Undefined ingredient
            Recipe: #{recipe_name}
            Ingredient: #{ingredient_name}
            Ingredient type: #{ingredient_type}
            """
          )
        end

        ingredient_definition = Map.fetch!(ingredients, ingredient_name)

        :ok = validate_recipe_ingredient!(
          ingredient_type, 
          recipe_name,
          ingredient_name,
          recipe_ingredient,
          ingredient_definition
        )
      end)
    end)
  end

  def validate_recipe_ingredient!("technical" = ingredient_type,
    recipe_name, ingredient_name, recipe_ingredient, ingredient_definition
  ) do
    case Map.fetch!(recipe_ingredient, "quantity") do
      "ANY" -> :ok
      _quantity ->
        unit = Map.fetch!(recipe_ingredient, "unit")
        defined_units = Map.fetch!(ingredient_definition, "units")

        unless unit in defined_units do
          raise(
            """
            Ingredient unit not defined
            Recipe: #{recipe_name}
            Ingredient: #{ingredient_name}
            Ingredient type: #{ingredient_type}
            Unit: #{unit}
            Defined units: #{inspect defined_units}
            """
          )
        end
    end

    :ok
  end

  def validate_recipe_ingredient!("regular" = ingredient_type,
    recipe_name, ingredient_name, recipe_ingredient, ingredient_definition
  ) do
    unit = Map.fetch!(recipe_ingredient, "unit")
    defined_units = Map.keys(Map.fetch!(ingredient_definition, "units"))

    unless unit in defined_units do
      raise(
        """
        Ingredient unit not defined
        Recipe: #{recipe_name}
        Ingredient: #{ingredient_name}
        Ingredient type: #{ingredient_type}
        Unit: #{unit}
        Defined units: #{inspect defined_units}
        """
      )
    end

    :ok
  end

  def validate_recipe_steps!(recipe_name, steps) do
    Enum.each(steps, fn({lang, steps}) ->
      unless Map.has_key?(steps, recipe_name) do
        raise(
          """
          Recipe steps not defined
          Recipe: #{recipe_name}
          Lang: #{lang}
          """
        )
      end

      recipe_steps = Map.fetch!(steps, recipe_name)

      unless is_list(recipe_steps) && Enum.all?(recipe_steps, &is_binary/1) do
        raise(
          """
          Recipe steps malformed
          Recipe: #{recipe_name}
          Lang: #{lang}
          """
        )
      end
    end)
  end

  def validate_code!(code, codes, langs) do
    Enum.each(langs, fn(lang) ->
      Logger.debug("Validating code #{code}. Lang: #{lang}")

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
    Logger.info("Validating known codes")

    recipe_types = ["BREAKFAST"]
    ingresient_characteristics = [
      "CALORIES", "PROTEINS", "FATS", "CARBOHYDRATES"
    ]

    known_codes = (
      recipe_types ++ ingresient_characteristics
    )

    Enum.each(known_codes, fn(code) ->
      validate_code!(code, codes, langs)
    end)
  end

  def validate_schema!(entity, schema) do
    JsonXema.validate!(schema, entity)
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
