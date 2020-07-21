:- module(recipe, [
    breakfast/1,
    snack/1,
    variant/5
]).

:- use_module(recipes_kb, [
    meal/2,
    sufficient/1,
    main_ingredients/2,
    additional_ingredients/3,
    complements/3
]).

:- use_module(ingredient, [
    nutrition_query/5
]).


breakfast(Recipe) :-
    generic_meal(Recipe, "BREAKFAST").

snack(Recipe) :-
    generic_meal(Recipe, "SNACK").

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
    ingredients_nutritions(AdditionalIngredients, [ACals, AProts, AFats, ACarbs]),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, [MCals, MProts, MFats, MCarbs]),

    Cals is ACals + MCals,
    Prots is AProts + MProts,
    Fats is AFats + MFats,
    Carbs is ACarbs + MCarbs,

    Nutritions = [Cals, Prots, Fats, Carbs].
variant(
    Recipe, Nutritions, none, ComplementsId, Excluded
) :-
    recipes_kb:sufficient(Recipe),

    recipes_kb:complements(
        Recipe, ComplementsId, Complements
    ), allowed_complements(Complements, Excluded),
    complements_nutritions(Complements, [CCals, CProts, CFats, CCarbs]),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, [MCals, MProts, MFats, MCarbs]),

    Cals is CCals + MCals,
    Prots is CProts + MProts,
    Fats is CFats + MFats,
    Carbs is CCarbs + MCarbs,

    Nutritions = [Cals, Prots, Fats, Carbs].
variant(
    Recipe, Nutritions, AdditionalIngredientsId, ComplementsId, Excluded
) :-
    recipes_kb:complements(
        Recipe, ComplementsId, Complements
    ), allowed_complements(Complements, Excluded),
    complements_nutritions(Complements, [CCals, CProts, CFats, CCarbs]),

    recipes_kb:additional_ingredients(
        Recipe, AdditionalIngredientsId, AdditionalIngredients
    ), allowed_ingredients(AdditionalIngredients, Excluded),
    ingredients_nutritions(AdditionalIngredients, [ACals, AProts, AFats, ACarbs]),

    recipes_kb:main_ingredients(
        Recipe, MainIngredients
    ), allowed_ingredients(MainIngredients, Excluded),
    ingredients_nutritions(MainIngredients, [MCals, MProts, MFats, MCarbs]),

    Cals is ACals + MCals + CCals,
    Prots is AProts + MProts + CProts,
    Fats is AFats + MFats + CFats,
    Carbs is ACarbs + MCarbs + CCarbs,

    Nutritions = [Cals, Prots, Fats, Carbs].


complements_nutritions([], [0,0,0,0]).
complements_nutritions([Complement|Complements], Nutritions) :-
    [Recipe, none] = Complement,

    recipes_kb:main_ingredients(Recipe, MainIngredients),
    ingredients_nutritions(MainIngredients, [CurCals, CurProts, CurFats, CurCarbs]),

    complements_nutritions(Complements, [AccCals, AccProts, AccFats, AccCarbs]),

    Cals is ceil(CurCals + AccCals),
    Prots is ceil(CurProts + AccProts),
    Fats is ceil(CurFats + AccFats),
    Carbs is ceil(CurCarbs + AccCarbs),

    Nutritions = [Cals, Prots, Fats, Carbs].
complements_nutritions([Complement|Complements], Nutritions) :-
    [Recipe, AdditionalIngredientsId] = Complement,

    recipes_kb:additional_ingredients(
        Recipe, AdditionalIngredientsId, AdditionalIngredients
    ),
    ingredients_nutritions(AdditionalIngredients, [ACals, AProts, AFats, ACarbs]),

    recipes_kb:main_ingredients(Recipe, MainIngredients),
    ingredients_nutritions(MainIngredients, [MCals, MProts, MFats, MCarbs]),

    CurCals is ACals + MCals,
    CurProts is AProts + MProts,
    CurFats is AFats + MFats,
    CurCarbs is ACarbs + MCarbs,

    complements_nutritions(Complements, [AccCals, AccProts, AccFats, AccCarbs]),

    Cals is ceil(CurCals + AccCals),
    Prots is ceil(CurProts + AccProts),
    Fats is ceil(CurFats + AccFats),
    Carbs is ceil(CurCarbs + AccCarbs),

    Nutritions = [Cals, Prots, Fats, Carbs].


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
