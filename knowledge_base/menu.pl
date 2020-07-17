:- set_prolog_flag(verbose, silent).

:- initialization main.

:- use_module(recipe, [
    breakfast/1,
    variant/3
]).


main :-
    % current_prolog_flag(argv, Args), print(Args),
    args(Nutritions, ExcludedRecipes),
    findall([Breakfast, Snack], menu(
        Breakfast, Snack, Nutritions, ExcludedRecipes
    ), Menu),
    print_menu(Menu),
    halt(0).


menu(Breakfast, Snack, TargetNutritions, ExcludedRecipes) :-
    meal(breakfast, Breakfast, ExcludedRecipes),
    [BR, BN, _] = Breakfast,
    meal(snack, Snack, ExcludedRecipes),
    [SR, SN, _] = Snack, SR \= BR,
    menu_nutritions(BN, SN, MN),
    check_nutritions(MN, TargetNutritions).


combine_nutritions(NCur, NAcc, NRes) :-
    [CCals, CProts, CFats, CCarbs] = NCur,
    [ACals, AProts, AFats, ACarbs] = NAcc,

    RCals is CCals + ACals,
    RProts is CProts + AProts,
    RFats is CFats + AFats,
    RCarbs is CCarbs + ACarbs,

    NRes = [RCals, RProts, RFats, RCarbs].


menu_nutritions(BN, SN, MN) :-
    apply:foldl(menu:combine_nutritions, [BN, SN], [0,0,0,0], MN).


check_nutritions(MenuNutritions, TargetNutritions) :-
    [BFCals, BFProts, BFFats, BFCarbs] = MenuNutritions,

    [TCals, TProts, TFats, TCarbs] = TargetNutritions,

    Cals is BFCals, Cals < TCals,
    Prots is BFProts, Prots < TProts,
    Fats is BFFats, Fats < TFats,
    Carbs is BFCarbs, Carbs < TCarbs.


meal(breakfast, Meal, ExcludedRecipes) :-
    recipe:breakfast(Recipe), \+ member(Recipe, ExcludedRecipes),
    recipe:variant(Recipe, Nutritions, AdditionalIngredientsId),

    Meal = [Recipe, Nutritions, AdditionalIngredientsId].
meal(snack, Meal, ExcludedRecipes) :-
    recipe:snack(Recipe), \+ member(Recipe, ExcludedRecipes),
    recipe:variant(Recipe, Nutritions, AdditionalIngredientsId),

    Meal = [Recipe, Nutritions, AdditionalIngredientsId].


print_menu([]) :- true.
print_menu([Menu|Menus]) :-
    [Breakfast, Snack] = Menu,
    format_recipe(Breakfast, BreakfastTxt),
    format_recipe(Snack, SnackTxt),
    format('{"breakfast": ~s, "snack": ~s}', [BreakfastTxt, SnackTxt]),
    print_menu(Menus).


format_recipe([Recipe, _Nutritions, none], Txt) :-
    swritef(Txt, '{"recipe": "%w", "additional_ingredients": null}', [Recipe]).
format_recipe([Recipe, _Nutritions, AdditionalIngredientsId], Txt) :-
    swritef(Txt, '{"recipe": "%w", "additional_ingredients": "%w"}', [
        Recipe, AdditionalIngredientsId
    ]).


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
