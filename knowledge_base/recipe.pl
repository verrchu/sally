:- module(recipe, [
    breakfast/1, snack/1, lunch/1,
    instance/5
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

    recipes_kb:ingredients(
        Recipe, Ingredients
    ), allowed_ingredients(Ingredients, Excluded),

    allowed_complements(Complements, Excluded).
allowed_complements([[Recipe,VariantId]|Complements], Excluded) :-
    allowed_recipe(Recipe, Excluded),

    embeddable_variant(Recipe, VariantId, VariantIngredients),
    allowed_ingredients(VariantIngredients, Excluded),

    recipes_kb:ingredients(
        Recipe, Ingredients
    ), allowed_ingredients(Ingredients, Excluded),

    allowed_complements(Complements, Excluded).


standalone_variant(Recipe, VariantId, VariantIngredients) :-
    recipes_kb:variant(Recipe, [standalone, VariantId], VariantIngredients).

embeddable_variant(Recipe, VariantId, VariantIngredients) :-
    recipes_kb:variant(Recipe, [embeddable, VariantId], VariantIngredients).

instance(
    Recipe, Nutritions, none, none, Excluded
) :-
    recipes_kb:sufficient(Recipe),

    recipes_kb:ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, Nutritions).
instance(
    Recipe, Nutritions, VariantId, none, Excluded
) :-
    standalone_variant(Recipe, VariantId, VariantIngredients),
    allowed_ingredients(VariantIngredients, Excluded),
    ingredients_nutritions(VariantIngredients, VNutritions),

    recipes_kb:ingredients(
        Recipe, Ingredients
    ), allowed_ingredients(Ingredients, Excluded),
    ingredients_nutritions(Ingredients, INutritions),

    nutritions:combine([VNutritions, INutritions], Nutritions).
instance(
    Recipe, Nutritions, none, ComplementsId, Excluded
) :-
    recipes_kb:sufficient(Recipe),

    recipes_kb:complements(
        Recipe, ComplementsId, Complements
    ), allowed_complements(Complements, Excluded),
    complements_nutritions(Complements, CNutritions),

    recipes_kb:ingredients(
        Recipe, Ingredients
    ), allowed_ingredients(Ingredients, Excluded),
    ingredients_nutritions(Ingredients, INutritions),

    nutritions:combine([INutritions, CNutritions], Nutritions).
instance(
    Recipe, Nutritions, VariantId, ComplementsId, Excluded
) :-
    recipes_kb:complements(
        Recipe, ComplementsId, Complements
    ), allowed_complements(Complements, Excluded),
    complements_nutritions(Complements, CNutritions),

    standalone_variant(Recipe, VariantId, VariantIngredients),
    allowed_ingredients(VariantIngredients, Excluded),
    ingredients_nutritions(VariantIngredients, VNutritions),

    recipes_kb:ingredients(
        Recipe, Ingredients
    ), allowed_ingredients(Ingredients, Excluded),
    ingredients_nutritions(Ingredients, INutritions),

    nutritions:combine([INutritions, VNutritions, CNutritions], Nutritions).


complements_nutritions([], Nutritions) :- nutritions:default(Nutritions).
complements_nutritions([Complement|Complements], Nutritions) :-
    [Recipe, none] = Complement,

    recipes_kb:ingredients(Recipe, Ingredients),
    ingredients_nutritions(Ingredients, INutritions),

    complements_nutritions(Complements, CNutritions),

    nutritions:combine([INutritions, CNutritions], Nutritions).
complements_nutritions([Complement|Complements], Nutritions) :-
    [Recipe, VariantId] = Complement,

    embeddable_variant(Recipe, VariantId, VariantIngredients),
    ingredients_nutritions(VariantIngredients, VNutritions),

    recipes_kb:ingredients(Recipe, Ingredients),
    ingredients_nutritions(Ingredients, INutritions),

    complements_nutritions(Complements, CNutritions),

    nutritions:combine([INutritions, VNutritions, CNutritions], Nutritions).


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
