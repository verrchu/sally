defmodule DataLoader do
  require Logger

  @localization_path ["localization"]
  @ingredients_path ["ingredients"]
  @recipes_path ["recipes"]
  @schemas_path ["schemas"]

  @ingredient_types ["regular", "technical"]

  @codes_file "codes.yaml"
  @steps_file "steps.yaml"
  @measures_file "measures.yaml"

  @schemas [
    measure: "measure.json",
    recipe: "recipe.json",
    regular_ingredient: "regular_ingredient.json",
    technical_ingredient: "technical_ingredient.json"
  ]

  def load_schemas!(data_dir) do
    schemas_path = path!([data_dir, @schemas_path])

    Logger.info("Loading schemas. Path: #{schemas_path}")

    Enum.reduce(@schemas, %{}, fn({name, path}, acc) ->
      Logger.debug("Loading schema #{inspect name}")

      schema_path = path!([schemas_path, path])
      schema = schema_path |> parse_json! |> JsonXema.new

      Map.put(acc, name, schema)
    end)
  end

  def load_codes!(data_dir, langs) do
    localization_path = path!([data_dir, @localization_path])

    Enum.reduce(langs, %{}, fn(lang, acc) ->
      codes_path = path!([localization_path, lang, @codes_file])

      Logger.info("Loading codes. Lang: #{lang}. Path: #{codes_path}")

      codes = parse_yaml!(codes_path)

      Map.put(acc, lang, codes)
    end)
  end

  def load_recipe_steps!(data_dir, langs) do
    localization_path = path!([data_dir, @localization_path])

    Enum.reduce(langs, %{}, fn(lang, acc) ->
      steps_path = path!([localization_path, lang, @steps_file])

      Logger.info("Loading recipe steps. Lang: #{lang}. Path: #{steps_path}")

      steps = parse_yaml!(steps_path)

      Map.put(acc, lang, steps)
    end)
  end

  def load_measures!(data_dir, langs) do
    localization_path = path!([data_dir, @localization_path])

    Enum.reduce(langs, %{}, fn(lang, acc) ->
      measures_path = path!([localization_path, lang, @measures_file])

      Logger.info("Loading measures. Lang: #{lang}. Path: #{measures_path}")

      measures = parse_yaml!(measures_path)

      Map.put(acc, lang, measures)
    end)
  end

  def load_ingredients!(data_dir) do
    Enum.reduce(@ingredient_types, %{}, fn(ingredients_type, acc) ->
      ingredients_path = path!([data_dir, @ingredients_path, ingredients_type])

      Logger.info(
        """
        Loading ingredients
        Type: #{ingredients_type}
        Path: #{ingredients_path}
        """
      )

      ingredients =
        Enum.reduce(File.ls!(ingredients_path), %{}, fn(file, acc) ->
          ingredient_file = path!([ingredients_path, file])
          ingredient_name = remove_file_ext(file) |> String.upcase

          Logger.debug("Loading ingredient #{ingredient_name}")

          ingredient = parse_yaml!(ingredient_file)

          Map.put(acc, ingredient_name, ingredient)
        end)

      Map.put(acc, ingredients_type, ingredients)
    end)
  end

  def load_recipes!(data_dir) do
    recipes_path = path!([data_dir, @recipes_path])

    Logger.info("Loading recipes. Path: #{recipes_path}")

    Enum.reduce(File.ls!(recipes_path), %{}, fn(file, acc) ->
      recipe_file = path!([recipes_path, file])
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

  defp path!(path_fragments) do
    path_fragments |> List.flatten |> Path.join
  end
end
