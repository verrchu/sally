:- module(recipe, [
    breakfast/1,
    characteristics_query/5
]).

:- use_module(recipes_kb, [
    type/2,
    ingredients/2
]).

:- use_module(ingredient, [
    characteristic_query/5
]).

breakfast(Breakfast) :-
    recipes_kb:type(Breakfast, 'BREAKFAST').

characteristics_query(Recipe, Cals, Prots, Fats, Carbs) :-
    recipes_kb:ingredients(Recipe, Ingredients),
    ingredients_characteristics(Ingredients, Cals, Prots, Fats, Carbs).

ingredients_characteristics([], 0, 0, 0, 0).
ingredients_characteristics(
    [Ingredient|Ingredients], Cals, Prots, Fats, Carbs
) :-
    [Name, Unit, Quantity] = Ingredient,

    ingredient:characteristic_query(Name, calories, Unit, Quantity, CurCals),
    ingredient:characteristic_query(Name, proteins, Unit, Quantity, CurProts),
    ingredient:characteristic_query(Name, fats, Unit, Quantity, CurFats),
    ingredient:characteristic_query(Name, carbohydrates, Unit, Quantity, CurCarbs),

    ingredients_characteristics(Ingredients, AccCals, AccProts, AccFats, AccCarbs),

    Cals is ceil(CurCals + AccCals),
    Prots is ceil(CurProts + AccProts),
    Fats is ceil(CurFats + AccFats),
    Carbs is ceil(CurCarbs + AccCarbs).
