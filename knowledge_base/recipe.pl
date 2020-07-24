:- module(recipe, [
    breakfast/1,
    snack/1,
    variant/5
]).

:- use_module(recipes_kb).
:- use_module(ingredient).
:- use_module(nutritions).


breakfast(Recipe) :-
    generic_meal(Recipe, "BREAKFAST").

snack(Recipe) :-
    generic_meal(Recipe, "SNACK").

lunch(Recipe) :-
    generic_meal(Recipe, "LUNCH").

generic_meal(Recipe, Meal) :-
    recipes_kb:meal(Recipe, Meal).


allowed_recipe(Recipe, [ExcludedRecipes, _]) :-
    \+ member(Recipe, ExcludedRecipes).

allowed_ingredients([], _).
allowed_ingredients([Ingredient|Ingredients], Excluded) :-
    Excluded = [_, ExcludedIngredients],
    Ingredient = [Name, _, _],

    \+ member(Name, ExcludedIngredients),

    allowed_ingredients(Ingredients, Excluded).

allowed_complements([], _).
allowed_complements([[Recipe,none]|Complements], Excluded) :-
    allowed_recipe(Recipe, Excluded),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),

    allowed_complements(Complements, Excluded).
allowed_complements([[Recipe,AdditionalIngredientsId]|Complements], Excluded) :-
    allowed_recipe(Recipe, Excluded),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),

    recipes_kb:additional_ingredients(
        Recipe, AdditionalIngredientsId, AdditionalIngredients
    ), allowed_ingredients(AdditionalIngredients, Excluded),

    allowed_complements(Complements, Excluded).


variant(
    Recipe, Nutritions, none, none, Excluded
) :-
    recipes_kb:sufficient(Recipe),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, Nutritions).
variant(
    Recipe, Nutritions, AdditionalIngredientsId, none, Excluded
) :-
    recipes_kb:additional_ingredients(
        Recipe, AdditionalIngredientsId, AdditionalIngredients
    ), allowed_ingredients(AdditionalIngredients, Excluded),
    ingredients_nutritions(AdditionalIngredients, AINutritions),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, MINutritions),

    nutritions:combine([AINutritions, MINutritions], Nutritions).
variant(
    Recipe, Nutritions, none, ComplementsId, Excluded
) :-
    recipes_kb:sufficient(Recipe),

    recipes_kb:complements(
        Recipe, ComplementsId, Complements
    ), allowed_complements(Complements, Excluded),
    complements_nutritions(Complements, CNutritions),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, MINutritions),

    nutritions:combine([MINutritions, CNutritions], Nutritions).
variant(
    Recipe, Nutritions, AdditionalIngredientsId, ComplementsId, Excluded
) :-
    recipes_kb:complements(
        Recipe, ComplementsId, Complements
    ), allowed_complements(Complements, Excluded),
    complements_nutritions(Complements, CNutritions),

    recipes_kb:additional_ingredients(
        Recipe, AdditionalIngredientsId, AdditionalIngredients
    ), allowed_ingredients(AdditionalIngredients, Excluded),
    ingredients_nutritions(AdditionalIngredients, AINutritions),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, MINutritions),

    nutritions:combine([MINutritions, AINutritions, CNutritions], Nutritions).


complements_nutritions([], Nutritions) :- nutritions:default(Nutritions).
complements_nutritions([Complement|Complements], Nutritions) :-
    [Recipe, none] = Complement,

    recipes_kb:main_ingredients(Recipe, MainIngredients),
    ingredients_nutritions(MainIngredients, MINutritions),

    complements_nutritions(Complements, CNutritions),

    nutritions:combine([MINutritions, CNutritions], Nutritions).
complements_nutritions([Complement|Complements], Nutritions) :-
    [Recipe, AdditionalIngredientsId] = Complement,

    recipes_kb:additional_ingredients(
        Recipe, AdditionalIngredientsId, AdditionalIngredients
    ),
    ingredients_nutritions(AdditionalIngredients, AINutritions),

    recipes_kb:main_ingredients(Recipe, MainIngredients),
    ingredients_nutritions(MainIngredients, MINutritions),

    complements_nutritions(Complements, CNutritions),

    nutritions:combine([MINutritions, AINutritions, CNutritions], Nutritions).


ingredients_nutritions([], Nutritions) :- nutritions:default(Nutritions).
ingredients_nutritions(
    [Ingredient|Ingredients], Nutritions
) :-
    [Name, Unit, Quantity] = Ingredient,

    ingredient:nutrition_query(Name, Unit, Quantity, calories, Cals),
    ingredient:nutrition_query(Name, Unit, Quantity, proteins, Prots),
    ingredient:nutrition_query(Name, Unit, Quantity, fats, Fats),
    ingredient:nutrition_query(Name, Unit, Quantity, carbohydrates, Carbs),

    nutritions:new(cals(Cals), prots(Prots), fats(Fats), carbs(Carbs), CurNutritions),

    ingredients_nutritions(Ingredients, AccNutritions),

    nutritions:combine([CurNutritions, AccNutritions], Nutritions).
