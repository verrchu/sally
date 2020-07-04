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
    @kb_file "ingredients_kb.pl"

    @predicate_characteristic "characteristic"

    @module_definition """
    :- module(ingredients_kb, [
        #{@predicate_characteristic}/5
    ]).
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
    end

    defp render_ingredient_characteristic(ingredient_name, ingredient, [file: file]) do
      ingredient_units = Map.fetch!(ingredient, "units")
      Enum.each(ingredient_units, fn({unit, data}) ->
        quantity = Map.fetch!(data, "quantity")
        characteristics = Map.fetch!(data, "characteristics")

        Enum.each(characteristics, fn({characteristic, value}) ->
          pred = @predicate_characteristic
          name = "'#{ingredient_name}'"
          unit = "'#{unit}'"
          predicate_clause = "#{pred}(#{name},#{characteristic},#{unit},#{quantity},#{value}).\n"

          File.write!(file, predicate_clause, [:append])
        end)
      end)
    end
  end

  defmodule Recipes do
    @kb_file "recipes_kb.pl"

    @predicate_meal "meal"
    @predicate_ingredients "ingredients"

    @module_definition """
    :- module(recipes_kb, [
        #{@predicate_meal}/2,
        #{@predicate_ingredients}/2
    ]).
    """

    def render(recipes, [dir: dir]) do
      file = Path.join([dir, @kb_file])
      
      file |> File.rm_rf! |> File.touch!

      File.write!(file, @module_definition, [:append])

      File.write!(file, "\n", [:append])

      Enum.each(recipes, fn({recipe_name, recipe}) ->
        recipe_meal = Map.fetch!(recipe, "meal")
        recipe_name = "'#{recipe_name}'"
        recipe_meal = "'#{recipe_meal}'"
        predicate = @predicate_meal
        predicate_clause = "#{predicate}(#{recipe_name},#{recipe_meal}).\n"

        File.write!(file, predicate_clause, [:append])
      end)

      File.write!(file, "\n", [:append])

      Enum.each(recipes, fn({recipe_name, recipe}) ->
        recipe_ingredients =
          recipe |> Map.fetch!("ingredients") |> Map.fetch!("main")
        ingredients = render_recipe_ingredients(recipe_ingredients)
        recipe_name = "'#{recipe_name}'"
        predicate = @predicate_ingredients
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
