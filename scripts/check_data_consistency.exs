defmodule Script do
  require Logger

  @recipes_path "recipes"
  @localization_path "localization"
  @steps_path "steps"
  @codes_file "codes.yaml"

  @types ['BREAKFAST']
  @units ['GRAM', 'NATURAL']

  @langs ["ru", "en"]

  def main([data_dir]) do
    Logger.info("Checking data consistency in #{data_dir}")
    recipes_path = Path.join(data_dir, @recipes_path)
    codes = load_codes(data_dir)
    recipe_files = File.ls!(recipes_path)
    Logger.info("Found recipe files: #{recipe_files}")
    Enum.each(recipe_files, fn(file_name) ->
      Process.spawn(fn() ->
        nil = Process.put(:codes, codes)
        nil = Process.put(:file_name, file_name)
        recipe = parse_file(Path.join(recipes_path, file_name))
        __MODULE__.check_recipe!(recipe)
      end, [:link])
    end)
  end

  defp load_codes(data_dir) do
    Enum.reduce(@langs, %{}, fn(lang, acc) ->
      Logger.debug("Loading codes for language #{lang}")
      codes = parse_file(Path.join([
        data_dir, @localization_path, lang, @codes_file
      ]))
      Map.put(acc, lang, codes)
    end)
  end

  def check_recipe!(recipe) do
    Logger.info("Checking recipe #{Process.get(:file_name)}")

    recipe = Enum.into(recipe, %{})

    true = recipe |> Map.fetch!('type') |> valid?(:type)
    :ok = recipe |> Map.fetch!('name') |> check_code!
    :ok = recipe |> Map.fetch!('ingredients') |> Enum.each(fn(ingredient) ->
      :ok = check_ingredient!(ingredient)
    end)
  end

  defp valid?(value, :type), do: value in @types
  defp valid?(value, :unit), do: value in @units

  defp check_code!(code) do
    Enum.each(@langs, fn(lang) ->
      try do
        Logger.debug("Checking code #{code} for language #{lang}")
        Process.get(:codes) |> Map.fetch!(lang) |> Map.fetch!(code)
      rescue
        err in KeyError ->
          Logger.error("Code #{err.key} is not defined for language #{lang}")
          raise(err)
      end
    end)
  end

  defp check_ingredient!({name, data}) do
    check_code!(name)

    data = Enum.into(data, %{})
    case Map.fetch!(data, 'quantity') do
      'ANY' -> :ok
      quantity when is_number(quantity) ->
        true = Map.fetch!(data, 'unit') |> valid?(:unit)
        max = Map.fetch!(data, 'max')
        min = Map.fetch!(data, 'min')
        true = is_number(min) && is_number(max) && min > 0 && max > min

        :ok
    end
  end

  defp parse_file(path) do
    path
    |> File.read!
    |> :yamerl.decode
    |> List.flatten
    |> Enum.into(%{})
  end
end

:ok = Application.start(:yamerl)

Script.main(System.argv())
