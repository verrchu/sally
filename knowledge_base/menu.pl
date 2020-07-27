:- set_prolog_flag(verbose, silent).

:- use_module(recipe, [
    breakfast/1, snack/1, lunch/1, dinner/1
]).
:- use_module(nutritions, except([default/1])).
:- use_module(excluded, except([default/1])).


main :-
    % current_prolog_flag(argv, Args), print(Args),
    args(TargetNutritions, Excluded),
    findall([[Breakfast, Snack, Lunch, Dinner], MenuNutritions], menu(
        [Breakfast, Snack, Lunch, Dinner],
        MenuNutritions, TargetNutritions, Excluded
    ), Menu),
    print_menu(Menu),
    halt(0).


menu([Breakfast, Snack, Lunch, Dinner], MenuNutritions, TargetNutritions, Excluded) :-
    meal(breakfast, Breakfast, Excluded),
    [BR, BN, _, _] = Breakfast,

    meal(snack, Snack, Excluded),
    [SR, SN, _, _] = Snack, SR \= BR,

    meal(lunch, Lunch, Excluded),
    [LR, LN, _, _] = Lunch, LR \= BR, LR \= SR,

    meal(dinner, Dinner, Excluded),
    [DR, DN, _, _] = Dinner, DR \= LR, DR \= BR, DR \= SR,

    nutritions:combine([BN, SN, LN, DN], MenuNutritions),

    check_nutritions(MenuNutritions, TargetNutritions).


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


meal(MealType, Meal, Excluded) :-
    call(MealType, Recipe),
    recipe:instance(
        Recipe, Nutritions, VariantId, ComplementsId, Excluded
    ),

    Meal = [Recipe, Nutritions, VariantId, ComplementsId].


print_menu([]) :- true.
print_menu([Menu|Menus]) :-
    [[Breakfast, Snack, Lunch, Dinner], Nutritions] = Menu,
    format_recipe(Breakfast, BreakfastTxt),
    format_recipe(Snack, SnackTxt),
    format_recipe(Lunch, LunchTxt),
    format_recipe(Dinner, DinnerTxt),
    format_nutritions(Nutritions, NutritionsTxt),
    format('{"meals": {"breakfast": ~s, "snack": ~s, "lunch": ~s, "dinner": ~s}, "nutritions": ~s}', [
        BreakfastTxt, SnackTxt, LunchTxt, DinnerTxt, NutritionsTxt
    ]),
    print_menu(Menus).


format_nutritions(Nutritions, Txt) :-
    nutritions:cals(Nutritions, Cals),
    nutritions:prots(Nutritions, Prots),
    nutritions:fats(Nutritions, Fats),
    nutritions:carbs(Nutritions, Carbs),

    format(string(CalsTxt), "~2f", [Cals]),
    format(string(ProtsTxt), "~2f", [Prots]),
    format(string(FatsTxt), "~2f", [Fats]),
    format(string(CarbsTxt), "~2f", [Carbs]),

    swritef(Txt, '{"calories": %w, "proteins": %w, "fats": %w, "carbohydrates": %w}', [
        CalsTxt, ProtsTxt, FatsTxt, CarbsTxt
    ]).

format_recipe([Recipe, _Nutritions, none, none], Txt) :-
    swritef(Txt, '{"recipe": "%w", "variant": null, "complements": null}', [
        Recipe
    ]).
format_recipe([Recipe, _Nutritions, VariantId, none], Txt) :-
    swritef(Txt, '{"recipe": "%w", "variant": "%w", "complements": null}', [
        Recipe, VariantId
    ]).
format_recipe([Recipe, _Nutritions, none, ComplementsId], Txt) :-
    swritef(Txt, '{"recipe": "%w", "variant": null, "complements": "%w"}', [
        Recipe, ComplementsId
    ]).
format_recipe([Recipe, _Nutritions, VariantId, ComplementsId], Txt) :-
    swritef(Txt, '{"recipe": "%w", "variant": "%w", "complements": "%w"}', [
        Recipe, VariantId, ComplementsId
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

    atom_number(CalsRaw, Cals), assertion(Cals > 0),
    atom_number(ProtsRaw, Prots), assertion(Prots > 0),
    atom_number(FatsRaw, Fats), assertion(Fats > 0),
    atom_number(CarbsRaw, Carbs), assertion(Carbs > 0),

    nutritions:new(cals(Cals), prots(Prots), fats(Fats), carbs(Carbs), Nutritions),

    parse_excluded_items(ExcludedRecipesTxt, ExcludedRecipes),
    parse_excluded_items(ExcludedIngredientsTxt, ExcludedIngredients),

    excluded:new(
        recipes(ExcludedRecipes),
        ingredients(ExcludedIngredients),

        Excluded
    ).


parse_excluded_items('_', []).
parse_excluded_items(Txt, Items) :-
    split_string(Txt, ',', '', Items).
