defmodule Script do
  def main([data_dir, knowledge_base_dir]) do
    ingredients = DataLoader.load_ingredients!(data_dir)
    :ok = KnowledgeBase.render_ingredients(knowledge_base_dir, ingredients)

    recipes = DataLoader.load_recipes!(data_dir)
    :ok = KnowledgeBase.render_recipes(knowledge_base_dir, recipes)
  end
end

defmodule KnowledgeBase do
  @ingredients_file "ingredients.pl"
  @recipes_file "recipes.pl"

  @ingredient_characteristic_query """
  ingredient_characteristic_query(Ingredient, Characteristic, Unit, Quantity, Value) :-
     ingredient_characteristic(Ingredient, Characteristic, Unit, BaseQuantity, BaseValue),
     Value is Quantity / BaseQuantity * BaseValue.
  """

  def render_ingredients(knowledge_base_dir, ingredients) do
    path = Path.join([knowledge_base_dir, @ingredients_file])
    regular_ingredients = Map.fetch!(ingredients, "regular")

    File.rm_rf!(path)
    File.touch!(path)

    Enum.each(regular_ingredients, fn({ingredient_name, ingredient}) ->
      render_ingredient_characteristic(path, ingredient_name, ingredient)
    end)

    File.write!(path, "\n", [:append])

    File.write!(path, @ingredient_characteristic_query, [:append])
  end

  defp render_ingredient_characteristic(path, ingredient_name, ingredient) do
    ingredient_units = Map.fetch!(ingredient, "units")
    Enum.each(ingredient_units, fn({unit, data}) ->
      quantity = Map.fetch!(data, "quantity")
      characteristics = Map.fetch!(data, "characteristics")

      Enum.each(characteristics, fn({characteristic, value}) ->
        pred = "ingredient_characteristic"
        name = "'#{ingredient_name}'"
        unit = "'#{unit}'"
        predicate_clause = "#{pred}(#{name},#{characteristic},#{unit},#{quantity},#{value}).\n"
        File.write!(path, predicate_clause, [:append])
      end)
    end)
  end

  def render_recipes(knowledge_base_dir, recipes) do
    path = Path.join([knowledge_base_dir, @recipes_file])
    
    File.rm_rf!(path)
    File.touch!(path)

    Enum.each(recipes, fn({recipe_name, recipe}) ->
      recipe_type = Map.fetch!(recipe, "type")
      recipe_name = "'#{recipe_name}'"
      recipe_type = "'#{recipe_type}'"
      predicate = "recipe_type"
      predicate_clause = "#{predicate}(#{recipe_name},#{recipe_type}).\n"

      File.write!(path, predicate_clause, [:append])
    end)

    File.write!(path, "\n", [:append])

    Enum.each(recipes, fn({recipe_name, recipe}) ->
      recipe_ingredients =
        recipe |> Map.fetch!("ingredients") |> Map.fetch!("regular")
      ingredients = render_recipe_ingredients(recipe_ingredients)
      recipe_name = "'#{recipe_name}'"
      predicate = "recipe_ingredients"
      predicate_clause = "#{predicate}(#{recipe_name},#{ingredients}).\n"

      File.write!(path, predicate_clause, [:append])
    end)

    File.write!(path, "\n", [:append])
  end

  defp render_recipe_ingredients(ingredients) do
    raw = Enum.map(ingredients, fn({ingredient_name, ingredient}) ->
      name = "'#{ingredient_name}'"
      unit = "'#{Map.fetch!(ingredient, "unit")}'"
      quantity = Map.fetch!(ingredient, "quantity")
      min = Map.fetch!(ingredient, "min")
      max = Map.fetch!(ingredient, "max")
      "[#{name},#{unit},#{quantity},#{min},#{max}]"
    end) |> Enum.join(",\n    ")

    "[\n    #{raw}\n]"
  end
end

[data_dir, knowledge_base_dir] = System.argv()

[{DataLoader, _}] = Code.require_file(
  Path.join([File.cwd!(), "scripts", "util", "data_loader.exs"])
)

Script.main([data_dir, knowledge_base_dir])
