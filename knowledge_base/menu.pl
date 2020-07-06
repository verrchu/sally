:- set_prolog_flag(verbose, silent).

:- initialization main.

:- use_module(recipe, [
    breakfast/1,
    nutritions/3
]).


main :-
    % args(Cals, Prots, Fats, Carbs),
    findall([Breakfast], menu(Breakfast), Menu),
    print_menu(Menu),
    halt.


menu(Breakfast) :-
    meal(breakfast, Breakfast).


meal(breakfast, [Recipe, AdditionalIngredients, Nutritions]) :-
    recipe:breakfast(Recipe),
    recipe:nutritions(Recipe, AdditionalIngredients, Nutritions).


print_menu([]) :- true.
print_menu([Menu|Menus]) :-
    [Breakfast] = Menu,
    format_breakfast(Breakfast, BreakfastTxt),
    format('{"breakfast": ~s}', [BreakfastTxt]),
    print_menu(Menus).


format_breakfast([Recipe, AdditionalIngredients, Nutritions], Txt) :-
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


args(CalsNum, ProtsNum, FatsNum, CarbsNum) :-
    current_prolog_flag(argv, [_, Cals, Prots, Fats, Carbs]),
    atom_number(Cals, CalsNum), positive(CalsNum),
    atom_number(Prots, ProtsNum), positive(ProtsNum),
    atom_number(Fats, FatsNum), positive(FatsNum),
    atom_number(Carbs, CarbsNum), positive(CarbsNum).

positive(X) :- X > 0.
