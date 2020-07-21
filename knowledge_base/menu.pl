:- set_prolog_flag(verbose, silent).

:- use_module(recipe, [
    breakfast/1,
    snack/1,
    variant/5
]).


main :-
    % current_prolog_flag(argv, Args), print(Args),
    args(Nutritions, Excluded),
    findall([Breakfast, Snack], menu(
        Breakfast, Snack, Nutritions, Excluded
    ), Menu),
    print_menu(Menu),
    halt(0).


menu(Breakfast, Snack, TargetNutritions, Excluded) :-
    meal(breakfast, Breakfast, Excluded),
    [BR, BN, _, _] = Breakfast,

    meal(snack, Snack, Excluded),
    [SR, SN, _, _] = Snack, SR \= BR,

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


check_nutritions(_, _).
% check_nutritions(MenuNutritions, TargetNutritions) :-
%     [MCals, MProts, MFats, MCarbs] = MenuNutritions,
%     [TCals, TProts, TFats, TCarbs] = TargetNutritions,

%     check_calories(MCals, TCals),
%     check_proteins(MProts, TProts),
%     check_fats(MFats, TFats),
%     check_carbohydrates(MCarbs, TCarbs).


% constants can be tuned if needed
check_calories(Val, Target) :-
    Val > Target * 0.90, Val < Target * 1.02.
check_proteins(Val, Target) :-
    Val > Target * 0.90, Val < Target * 1.02.
check_fats(Val, Target) :-
    Val > Target * 0.90, Val < Target * 1.02.
check_carbohydrates(Val, Target) :-
    Val > Target * 0.90, Val < Target * 1.02.


% TODO: reduce code duplication
meal(breakfast, Meal, Excluded) :-
    recipe:breakfast(Recipe),
    recipe:variant(
        Recipe, Nutritions, AdditionalIngredientsId, ComplementsId, Excluded
    ),

    Meal = [Recipe, Nutritions, AdditionalIngredientsId, ComplementsId].
meal(snack, Meal, Excluded) :-
    recipe:snack(Recipe),
    recipe:variant(
        Recipe, Nutritions, AdditionalIngredientsId, ComplementsId, Excluded
    ),

    Meal = [Recipe, Nutritions, AdditionalIngredientsId, ComplementsId].


print_menu([]) :- true.
print_menu([Menu|Menus]) :-
    [Breakfast, Snack] = Menu,
    format_recipe(Breakfast, BreakfastTxt),
    format_recipe(Snack, SnackTxt),
    format('{"breakfast": ~s, "snack": ~s}', [BreakfastTxt, SnackTxt]),
    print_menu(Menus).


format_recipe([Recipe, _Nutritions, none, none], Txt) :-
    swritef(Txt, '{"recipe": "%w", "additional_ingredients": null, "complements": null}', [Recipe]).
format_recipe([Recipe, _Nutritions, AdditionalIngredientsId, none], Txt) :-
    swritef(Txt, '{"recipe": "%w", "additional_ingredients": "%w", "complements": null}', [
        Recipe, AdditionalIngredientsId
    ]).
format_recipe([Recipe, _Nutritions, none, ComplementsId], Txt) :-
    swritef(Txt, '{"recipe": "%w", "additional_ingredients": null, "complements": "%w"}', [
        Recipe, ComplementsId
    ]).
format_recipe([Recipe, _Nutritions, AdditionalIngredientsId, ComplementsId], Txt) :-
    swritef(Txt, '{"recipe": "%w", "additional_ingredients": "%w", "complements": "%w"}', [
        Recipe, AdditionalIngredientsId, ComplementsId
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


args(Nutritions, Excluded) :-
    current_prolog_flag(argv, [
        CalsRaw, ProtsRaw, FatsRaw, CarbsRaw, ExcludedRecipesTxt, ExcludedIngredientsTxt
    ]),

    atom_number(CalsRaw, Cals), positive(Cals),
    atom_number(ProtsRaw, Prots), positive(Prots),
    atom_number(FatsRaw, Fats), positive(Fats),
    atom_number(CarbsRaw, Carbs), positive(Carbs),

    Nutritions = [Cals, Prots, Fats, Carbs],

    parse_excluded_items(ExcludedRecipesTxt, ExcludedRecipes),
    parse_excluded_items(ExcludedIngredientsTxt, ExcludedIngredients),

    Excluded = [ExcludedRecipes, ExcludedIngredients].


parse_excluded_items("_", []).
parse_excluded_items(Txt, Items) :-
    split_string(Txt, ',', '', Items).


positive(X) :- X > 0.
