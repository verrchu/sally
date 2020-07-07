:- set_prolog_flag(verbose, silent).

:- initialization main.

:- use_module(recipe, [
    breakfast/1,
    variant/3
]).


main :-
    args(Nutritions, ExcludedRecipes),
    findall([Breakfast], menu(
        Breakfast, Nutritions, ExcludedRecipes
    ), Menu),
    print_menu(Menu),
    halt(0).
main :-
    halt(1).


menu(Breakfast, Nutritions, ExcludedRecipes) :-
    meal(breakfast, Breakfast, ExcludedRecipes),
    [_, _, BreakfastNutritions] = Breakfast,
    check_nutritions(BreakfastNutritions, Nutritions).


check_nutritions(BreakfastNutritions, TargetNutritions) :-
    [BFCals, BFProts, BFFats, BFCarbs] = BreakfastNutritions,

    [TCals, TProts, TFats, TCarbs] = TargetNutritions,

    Cals is BFCals, Cals < TCals,
    Prots is BFProts, Prots < TProts,
    Fats is BFFats, Fats < TFats,
    Carbs is BFCarbs, Carbs < TCarbs.


meal(breakfast, [Recipe, AdditionalIngredients, Nutritions], ExcludedRecipes) :-
    recipe:breakfast(Recipe),
    atom_string(Recipe, RecipeStr), \+ member(RecipeStr, ExcludedRecipes),
    recipe:variant(Recipe, AdditionalIngredients, Nutritions).


print_menu([]) :- true.
print_menu([Menu|Menus]) :-
    [Breakfast] = Menu,
    format_breakfast(Breakfast, BreakfastTxt),
    format('{"breakfast": ~s}', [BreakfastTxt]),
    print_menu(Menus).


format_breakfast([Recipe, AdditionalIngredients, _Nutritions], Txt) :-
    format_ingredients(AdditionalIngredients, AdditionalIngredientsTxt),
    swritef(
        Txt,
        '{"recipe": "%w", "additional_ingredients": [%w]}',
        [Recipe, AdditionalIngredientsTxt]
    ).


format_ingredients([], '').
format_ingredients([Ingredient], Txt) :- format_ingredient(Ingredient, Txt).
format_ingredients([Ingredient|Ingredients], Txt) :-
    format_ingredient(Ingredient, IngredientTxt),
    format_ingredients(Ingredients, IngredientsTxt),
    swritef(Txt, '%w,%w', [IngredientTxt, IngredientsTxt]).


format_ingredient([Name, Unit, Quantity], Txt) :-
    swritef(
        Txt,
        '{"name": "%w", "unit": "%w", "quantity": %d}',
        [Name, Unit, Quantity]
    ).


args(Nutritions, ExcludedRecipes) :-
    current_prolog_flag(argv, [
        CalsRaw, ProtsRaw, FatsRaw, CarbsRaw, ExcludedRecipesRaw
    ]),

    atom_number(CalsRaw, Cals), positive(Cals),
    atom_number(ProtsRaw, Prots), positive(Prots),
    atom_number(FatsRaw, Fats), positive(Fats),
    atom_number(CarbsRaw, Carbs), positive(Carbs),

    Nutritions = [Cals, Prots, Fats, Carbs],
    split_string(ExcludedRecipesRaw, ',', '', ExcludedRecipes).

positive(X) :- X > 0.
