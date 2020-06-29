defmodule Script do
  def main([data_dir, knowledge_base_dir]) do
    ingredients = DataLoader.load_ingredients!(data_dir)
    :ok = KnowledgeBase.Ingredients.render(
      ingredients, [dir: knowledge_base_dir]
    )

    recipes = DataLoader.load_recipes!(data_dir)
    :ok = KnowledgeBase.Recipes.render(
      recipes, [dir: knowledge_base_dir]
    )
  end
end

defmodule KnowledgeBase do
  defmodule Ingredients do
    @kb_file "ingredients.pl"

    @module_definition """
    :- module(ingredients, [
        ingredient_characteristic/5,
        ingredient_characteristic_query/5
    ]).
    """

    @ingredient_characteristic_query """
    ingredient_characteristic_query(Ingredient, Characteristic, Unit, Quantity, Value) :-
       ingredient_characteristic(Ingredient, Characteristic, Unit, BaseQuantity, BaseValue),
       Value is Quantity / BaseQuantity * BaseValue.
    """

    def render(ingredients, [dir: dir]) do
      file = Path.join([dir, @kb_file])
      regular_ingredients = Map.fetch!(ingredients, "regular")

      file |> File.rm_rf! |> File.touch!

      File.write!(file, @module_definition, [:append])

      File.write!(file, "\n", [:append])

      Enum.each(regular_ingredients, fn({ingredient_name, ingredient}) ->
        render_ingredient_characteristic(
          ingredient_name, ingredient, [file: file]
        )
      end)

      File.write!(file, "\n", [:append])

      File.write!(file, @ingredient_characteristic_query, [:append])
    end

    defp render_ingredient_characteristic(ingredient_name, ingredient, [file: file]) do
      ingredient_units = Map.fetch!(ingredient, "units")
      Enum.each(ingredient_units, fn({unit, data}) ->
        quantity = Map.fetch!(data, "quantity")
        characteristics = Map.fetch!(data, "characteristics")

        Enum.each(characteristics, fn({characteristic, value}) ->
          pred = "ingredient_characteristic"
          name = "'#{ingredient_name}'"
          unit = "'#{unit}'"
          predicate_clause = "#{pred}(#{name},#{characteristic},#{unit},#{quantity},#{value}).\n"

          File.write!(file, predicate_clause, [:append])
        end)
      end)
    end
  end

  defmodule Recipes do
    @kb_file "recipes.pl"

    @module_definition """
    :- module(recipes, [
        recipe_type/2,
        recipe_ingredients/2
    ]).
    """

    def render(recipes, [dir: dir]) do
      file = Path.join([dir, @kb_file])
      
      file |> File.rm_rf! |> File.touch!

      File.write!(file, @module_definition, [:append])

      File.write!(file, "\n", [:append])

      Enum.each(recipes, fn({recipe_name, recipe}) ->
        recipe_type = Map.fetch!(recipe, "type")
        recipe_name = "'#{recipe_name}'"
        recipe_type = "'#{recipe_type}'"
        predicate = "recipe_type"
        predicate_clause = "#{predicate}(#{recipe_name},#{recipe_type}).\n"

        File.write!(file, predicate_clause, [:append])
      end)

      File.write!(file, "\n", [:append])

      Enum.each(recipes, fn({recipe_name, recipe}) ->
        recipe_ingredients =
          recipe |> Map.fetch!("ingredients") |> Map.fetch!("regular")
        ingredients = render_recipe_ingredients(recipe_ingredients)
        recipe_name = "'#{recipe_name}'"
        predicate = "recipe_ingredients"
        predicate_clause = "#{predicate}(#{recipe_name},#{ingredients}).\n"

        File.write!(file, predicate_clause, [:append])
      end)

      File.write!(file, "\n", [:append])
    end

    defp render_recipe_ingredients(ingredients) do
      raw = Enum.map(ingredients, fn({ingredient_name, ingredient}) ->
        name = "'#{ingredient_name}'"
        unit = "'#{Map.fetch!(ingredient, "unit")}'"
        quantity = Map.fetch!(ingredient, "quantity")
        "[#{name},#{unit},#{quantity}]"
      end) |> Enum.join(",\n    ")

      "[\n    #{raw}\n]"
    end
  end
end

[data_dir, knowledge_base_dir] = System.argv()

[{DataLoader, _}] = Code.require_file(
  Path.join([File.cwd!(), "scripts", "util", "data_loader.exs"])
)

Script.main([data_dir, knowledge_base_dir])
