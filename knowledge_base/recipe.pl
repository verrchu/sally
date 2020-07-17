:- module(recipe, [
    breakfast/1,
    snack/1,
    variant/3
]).

:- use_module(recipes_kb, [
    meal/2,
    sufficient/1,
    main_ingredients/2,
    additional_ingredients/3
]).

:- use_module(ingredient, [
    nutrition_query/5
]).


breakfast(Recipe) :-
    recipes_kb:meal(Recipe, "BREAKFAST").

snack(Recipe) :-
    recipes_kb:meal(Recipe, "SNACK").


variant(Recipe, Nutritions, AdditionalIngredientsId) :-
    recipes_kb:additional_ingredients(
        Recipe, AdditionalIngredientsId, AdditionalIngredients
    ),
    ingredients_nutritions(AdditionalIngredients, [ACals, AProts, AFats, ACarbs]),
    recipes_kb:main_ingredients(Recipe, MainIngredients),
    ingredients_nutritions(MainIngredients, [MCals, MProts, MFats, MCarbs]),

    Cals is ACals + MCals,
    Prots is AProts + MProts,
    Fats is AFats + MFats,
    Carbs is ACarbs + MCarbs,

    Nutritions = [Cals, Prots, Fats, Carbs].
variant(Recipe, Nutritions, none) :-
    recipes_kb:sufficient(Recipe),
    recipes_kb:main_ingredients(Recipe, MainIngredients),
    ingredients_nutritions(MainIngredients, Nutritions).


ingredients_nutritions([], [0, 0, 0, 0]).
ingredients_nutritions(
    [Ingredient|Ingredients], Nutritions
) :-
    [Name, Unit, Quantity] = Ingredient,

    ingredient:nutrition_query(Name, Unit, Quantity, calories, CurCals),
    ingredient:nutrition_query(Name, Unit, Quantity, proteins, CurProts),
    ingredient:nutrition_query(Name, Unit, Quantity, fats, CurFats),
    ingredient:nutrition_query(Name, Unit, Quantity, carbohydrates, CurCarbs),

    ingredients_nutritions(Ingredients, [AccCals, AccProts, AccFats, AccCarbs]),

    Cals is ceil(CurCals + AccCals),
    Prots is ceil(CurProts + AccProts),
    Fats is ceil(CurFats + AccFats),
    Carbs is ceil(CurCarbs + AccCarbs),

    Nutritions = [Cals, Prots, Fats, Carbs].
