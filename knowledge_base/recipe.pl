:- module(recipe, [
    breakfast/1,
    nutritions/5
]).

:- use_module(recipes_kb, [
    meal/2,
    main_ingredients/2
]).

:- use_module(ingredient, [
    nutrition_query/5
]).

breakfast(Breakfast) :-
    recipes_kb:meal(Breakfast, 'BREAKFAST').

nutritions(Recipe, Cals, Prots, Fats, Carbs) :-
    recipes_kb:main_ingredients(Recipe, Ingredients),
    ingredients_nutritions(Ingredients, Cals, Prots, Fats, Carbs).

ingredients_nutritions([], 0, 0, 0, 0).
ingredients_nutritions(
    [Ingredient|Ingredients], Cals, Prots, Fats, Carbs
) :-
    [Name, Unit, Quantity] = Ingredient,

    ingredient:nutrition_query(Name, Unit, Quantity, calories, CurCals),
    ingredient:nutrition_query(Name, Unit, Quantity, proteins, CurProts),
    ingredient:nutrition_query(Name, Unit, Quantity, fats, CurFats),
    ingredient:nutrition_query(Name, Unit, Quantity, carbohydrates, CurCarbs),

    ingredients_nutritions(Ingredients, AccCals, AccProts, AccFats, AccCarbs),

    Cals is ceil(CurCals + AccCals),
    Prots is ceil(CurProts + AccProts),
    Fats is ceil(CurFats + AccFats),
    Carbs is ceil(CurCarbs + AccCarbs).
