defmodule Script do
  require Logger

  @yaml_ext ".yaml"

  @recipes_path "recipes"
  @localization_path "localization"
  @ingredients_path "ingredients"

  @steps_file "steps.yaml"
  @codes_file "codes.yaml"

  @recipe_types ["BREAKFAST"]
  @ingredient_units ["GRAM", "NATURAL"]
  @ingresient_characteristics ["CALORIES", "PROTEINS", "FATS", "CARBOHYDRATES"]

  @void_codes ["NATURAL"]

  @langs ["ru", "en"]

  def main([data_dir]) do
    Logger.info("Checking data consistency in #{data_dir}")

    codes = preload_codes(data_dir)
    ingredients = preload_ingredients(data_dir)

    :ok = validate_known_codes(codes)

    :ok = validate_recipes(data_dir, ingredients, codes)
  end

  defp validate_recipes(data_dir, ingredients, codes) do
    recipes_path = Path.join(data_dir, @recipes_path)
    recipe_files = File.ls!(recipes_path)

    Logger.info("Found recipe files: #{inspect recipe_files}")
    
    true = Task.async_stream(recipe_files, fn(file) ->
      name = get_name_from_file(file)

      Logger.info("Validationg recipe #{name}")

      {:ok, recipe} = parse_file(Path.join(recipes_path, file))
      :ok = validate_recipe_declaration(name, recipe, ingredients, codes)
    end) |> Enum.all?(fn({:ok, :ok}) -> true end)

    :ok
  end

  defp validate_recipe_declaration(name, recipe, ingredients, codes) do
    :ok
  end

  defp validate_known_codes(codes) do
    knwon_codes = (
      @recipe_types ++ @ingredient_units ++ @ingresient_characteristics
    ) -- @void_codes
    Enum.each(knwon_codes, fn(code) -> validate_code(code, codes) end)
  end

  defp preload_ingredients(data_dir) do
    ingredients_path = Path.join(data_dir, @ingredients_path)
    ingredient_files = File.ls!(ingredients_path)

    Logger.debug("Found ingredient files: #{inspect ingredient_files}")

    Task.async_stream(ingredient_files, fn(file) ->
      name = get_name_from_file(file)

      Logger.debug("CHecking ingredient #{to_string(name)}")

      {:ok, data} = parse_file(Path.join(ingredients_path, file))
      :ok = validate_ingredient_declaration(name, data)

      name
    end) |> Enum.map(fn({:ok, name}) -> name end)
  end

  defp get_name_from_file(file) do
    [ingredient, _file_ext] = String.split(file, ".")
    ingredient = ingredient |> String.upcase |> String.to_charlist
  end

  defp validate_ingredient_declaration(name, data) do
    data = Enum.into(data, %{})

    units = Map.keys(data)
    unless MapSet.subset?(MapSet.new(units), MapSet.new(@ingredient_units)) do
      diff = MapSet.difference(MapSet.new(units), MapSet.new(@ingredient_units))
      log_and_raise("Ingredient declaration for #{name} contains invalid units #{inspect diff}")
    end

    Enum.each(Map.values(data), fn(declaration) ->
      unless Map.has_key?(declaration, "quantity") do
        log_and_raise("Ingredient #{name}: quantity not defined")
      end
      quantity = Map.get(declaration, "quantity")

      unless positive_number?(quantity) do
        log_and_raise("Ingredient #{name}: quantity shoul be a positive number")
      end

      unless Map.has_key?(declaration, "characteristics") do
        log_and_raise("Ingredient #{name}: characteristics not defined")
      end
      characteristics = Map.get(declaration, "characteristics")

      @ingresient_characteristics
      |> Enum.map(&String.downcase/1)
      |> Enum.each(fn(characteristic) ->
        unless Map.has_key?(characteristics, characteristic) do
          log_and_raise("Ingredient #{name}: #{characteristic} not defined")
        end

        unless non_negative_number?(characteristics[characteristic]) do
          log_and_raise("Ingredient #{name}: #{characteristic} should be a non negative number")
        end
      end)
    end)

    :ok
  end

  defp preload_codes(data_dir) do
    Enum.reduce(@langs, %{}, fn(lang, acc) ->
      Logger.debug("Loading codes for language #{lang}")
      {:ok, codes} = parse_file(Path.join([
        data_dir, @localization_path, lang, @codes_file
      ]))
      Map.put(acc, lang, codes)
    end)
  end

  # def check_recipe!(recipe) do
  #   Logger.info("Checking recipe #{Process.get(:file_name)}")

  #   recipe = Enum.into(recipe, %{})

  #   true = recipe |> Map.fetch!('type') |> valid?(:type)
  #   :ok = recipe |> Map.fetch!('name') |> check_code!
  #   :ok = recipe |> Map.fetch!('ingredients') |> Enum.each(fn(ingredient) ->
  #     :ok = check_ingredient!(ingredient)
  #   end)
  # end

  # defp valid?(value, :type), do: value in @recipe_types
  # defp valid?(value, :unit), do: value in @ingredient_units

  defp validate_code(code, codes) do
    Enum.each(@langs, fn(lang) ->
      try do
        Logger.debug("Checking code #{code} for language #{lang}")
        codes |> Map.fetch!(lang) |> Map.fetch!(code)
      rescue
        err in KeyError ->
          log_and_raise("Code #{err.key} is not defined for language #{lang}")
      end
    end)
  end

  # defp check_ingredient!({name, data}) do
  #   check_code!(name)

  #   data = Enum.into(data, %{})
  #   case Map.fetch!(data, 'quantity') do
  #     'ANY' -> :ok
  #     quantity when is_number(quantity) ->
  #       true = Map.fetch!(data, 'unit') |> valid?(:unit)
  #       max = Map.fetch!(data, 'max')
  #       min = Map.fetch!(data, 'min')
  #       true = is_number(min) && is_number(max) && min > 0 && max > min

  #       :ok
  #   end
  # end

  defp parse_file(path) do
    path |> File.read!  |> YamlElixir.read_from_string
  end

  defp positive_number?(n), do: is_number(n) && n > 0

  defp non_negative_number?(n), do: is_number(n) && n >= 0

  defp log_and_raise(err_msg), do: err_msg |> Logger.error |> raise
end

{:ok, _apps} = Application.ensure_all_started(:yaml_elixir)

Script.main(System.argv())
