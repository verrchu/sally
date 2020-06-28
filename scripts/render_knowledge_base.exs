defmodule Script do
  def main([data_dir, knowledge_base_dir]) do
    ingredients = DataLoader.load_ingredients!(data_dir)
    :ok = KnowledgeBase.render_ingredients(knowledge_base_dir, ingredients)
  end
end

defmodule KnowledgeBase do
  @ingredients_file "ingredients.pl"

  @ingredient_characteristic_query """
  ingredient_characteristic_query(Ingredient, Characteristic, Unit, Quantity, Value) :-
     ingredient_characteristic(Ingredient, Characteristic, Unit, BaseQuantity, BaseValue),
     Value is Quantity / BaseQuantity * BaseValue.
  """

  def render_ingredients(knowledge_base_dir, ingredients) do
    path = Path.join([knowledge_base_dir, @ingredients_file])
    regular_ingredients = Map.fetch!(ingredients, "regular")

    File.rm!(path)
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
        predicate_clause = """
        #{pred}(#{name},#{characteristic},#{unit},#{quantity},#{value}).
        """
        File.write!(path, predicate_clause, [:append])
      end)
    end)
  end
end

[data_dir, knowledge_base_dir] = System.argv()

[{DataLoader, _}] = Code.require_file(
  Path.join([File.cwd!(), "scripts", "util", "data_loader.exs"])
)

Script.main([data_dir, knowledge_base_dir])
