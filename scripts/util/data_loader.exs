defmodule DataLoader do
  require Logger

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

    Logger.info("Loading schemas. Path: #{schemas_path}")

    Enum.reduce(@schemas, %{}, fn({name, path}, acc) ->
      Logger.debug("Loading schema #{inspect name}")

      schema_path = Path.join(schemas_path, path)
      schema = schema_path |> parse_json! |> JsonXema.new

      Map.put(acc, name, schema)
    end)
  end

  def load_codes!(data_dir, langs) do
    localization_path = Path.join(data_dir, @localization_path)

    Enum.reduce(langs, %{}, fn(lang, acc) ->
      codes_path = Path.join([localization_path, lang, @codes_file])

      Logger.info("Loading codes. Lang: #{lang}. Path: #{codes_path}")

      codes = parse_yaml!(codes_path)

      Map.put(acc, lang, codes)
    end)
  end

  def load_recipe_steps!(data_dir, langs) do
    localization_path = Path.join(data_dir, @localization_path)

    Enum.reduce(langs, %{}, fn(lang, acc) ->
      steps_path = Path.join([localization_path, lang, @steps_file])

      Logger.info("Loading recipe steps. Lang: #{lang}. Path: #{steps_path}")

      steps = parse_yaml!(steps_path)

      Map.put(acc, lang, steps)
    end)
  end

  def load_ingredients!(data_dir) do
    ingredients_path = Path.join(data_dir, @ingredients_path)

    Logger.info("Loading ingredients. Path: #{ingredients_path}")

    Enum.reduce(File.ls!(ingredients_path), %{}, fn(file, acc) ->
      ingredient_file = Path.join(ingredients_path, file)
      ingredient_name = remove_file_ext(file) |> String.upcase

      Logger.debug("Loading ingredient #{ingredient_name}")

      ingredient = parse_yaml!(ingredient_file)

      Map.put(acc, ingredient_name, ingredient)
    end)
  end

  def load_recipes!(data_dir) do
    recipes_path = Path.join(data_dir, @recipes_path)

    Logger.info("Loading recipes. Path: #{recipes_path}")

    Enum.reduce(File.ls!(recipes_path), %{}, fn(file, acc) ->
      recipe_file = Path.join(recipes_path, file)
      recipe_name = remove_file_ext(file) |> String.upcase

      Logger.debug("Loading recipe #{recipe_name}")

      recipe = parse_yaml!(recipe_file)

      Map.put(acc, recipe_name, recipe)
    end)
  end

  defp parse_yaml!(path), do: parse_file!(path, :yaml)
  defp parse_json!(path), do: parse_file!(path, :json)

  defp parse_file!(path, :yaml) do
    path |> File.read! |> YamlElixir.read_from_string!
  end

  defp parse_file!(path, :json) do
    path |> File.read! |> Jason.decode!
  end

  def remove_file_ext(file), do: hd(String.split(file, "."))
end
