:- discontiguous plunit:setup_suite/1.
:- discontiguous plunit:cleanup_suite/1.
:- discontiguous plunit:alter_suite/2.


% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++++++ TEST RECIPE MEAL +++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(recipe_meal).

:- discontiguous plunit_recipe_meal:setup_test/1.
:- discontiguous plunit_recipe_meal:cleanup_test/1.
:- discontiguous plunit_recipe_meal:alter_test/2.

% ============================================================================ %

setup_test(breakfast) :-
    alter_test(breakfast, assert).

cleanup_test(breakfast) :-
    alter_test(breakfast, retract).

alter_test(breakfast, Pred) :-
    call(Pred, recipes_kb:meal("TEST_RECIPE", "BREAKFAST")).

test(breakfast, [
    setup(setup_test(breakfast)),
    cleanup(cleanup_test(breakfast))
]) :-
    recipe:breakfast("TEST_RECIPE").

% ============================================================================ %

setup_test(snack) :-
    alter_test(snack, assert).

cleanup_test(snack) :-
    alter_test(snack, retract).

alter_test(snack, Pred) :-
    call(Pred, recipes_kb:meal("TEST_RECIPE", "SNACK")).

test(snack, [
    setup(setup_test(snack)),
    cleanup(cleanup_test(snack))
]) :-
    recipe:snack("TEST_RECIPE").

% ============================================================================ %

setup_test(lunch) :-
    alter_test(lunch, assert).

cleanup_test(lunch) :-
    alter_test(lunch, retract).

alter_test(lunch, Pred) :-
    call(Pred, recipes_kb:meal("TEST_RECIPE", "LUNCH")).

test(lunch, [
    setup(setup_test(lunch)),
    cleanup(cleanup_test(lunch))
]) :-
    recipe:lunch("TEST_RECIPE").

% ============================================================================ %

setup_test(dinner) :-
    alter_test(dinner, assert).

cleanup_test(dinner) :-
    alter_test(dinner, retract).

alter_test(dinner, Pred) :-
    call(Pred, recipes_kb:meal("TEST_RECIPE", "DINNER")).

test(dinner, [
    setup(setup_test(dinner)),
    cleanup(cleanup_test(dinner))
]) :-
    recipe:dinner("TEST_RECIPE").

:- end_tests(recipe_meal).

% ---------------------------------------------------------------------------- %
% --------------------------- TEST RECIPE MEAL ------------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++ TEST INGREDIENT NUTRITION QUERY +++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(ingredient_nutrition_query).

:- discontiguous plunit_ingredient_nutrition_query:setup_test/1.
:- discontiguous plunit_ingredient_nutrition_query:cleanup_test/1.
:- discontiguous plunit_ingredient_nutrition_query:alter_test/2.

% ============================================================================ %

setup_test(single_unit) :-
    alter_test(single_unit, assert).

cleanup_test(single_unit) :-
    alter_test(single_unit, retract).

alter_test(single_unit, Pred) :-
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,proteins,5)).

test(single_unit, [
    setup(setup_test(single_unit)),
    cleanup(cleanup_test(single_unit))
]) :-
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,calories,Cals), assertion(Cals == 500),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,carbohydrates,Carbs), assertion(Carbs == 125),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,fats,Fats), assertion(Fats == 25),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,proteins,Prots), assertion(Prots == 25).

% ============================================================================ %

setup_test(multiple_units) :-
    alter_test(multiple_units, assert).

cleanup_test(multiple_units) :-
    alter_test(multiple_units, retract).

alter_test(multiple_units, Pred) :-
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,proteins,5)),

    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,proteins,5)).

test(multiple_units, [
    setup(setup_test(multiple_units)),
    cleanup(cleanup_test(multiple_units)),
    nondet
]) :-
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,calories,ACals), assertion(ACals == 500),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,carbohydrates,ACarbs), assertion(ACarbs == 125),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,fats,AFats), assertion(AFats == 25),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,proteins,AProts), assertion(AProts == 25),

    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,calories,BCals), assertion(BCals == 500),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,carbohydrates,BCarbs), assertion(BCarbs == 125),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,fats,BFats), assertion(BFats == 25),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,proteins,BProts), assertion(BProts == 25).

:- end_tests(ingredient_nutrition_query).

% ---------------------------------------------------------------------------- %
% -------------------- TEST INGREDIENT NUTRITION QUERY ----------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++ TEST RECIPE INGREDIENTS ++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

setup_suite(recipe_ingredients) :-
    alter_suite(recipe_ingredients, assert).

cleanup_suite(recipe_ingredients) :-
    alter_suite(recipe_ingredients, retract).

alter_suite(recipe_ingredients, Pred) :-
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,proteins,5)).

:- begin_tests(recipe_ingredients, [
    setup(setup_suite(recipe_ingredients)),
    cleanup(cleanup_suite(recipe_ingredients))
]).

setup_test(recipe_ingredients) :-
    alter_test(recipe_ingredients, assert).

cleanup_test(recipe_ingredients) :-
    alter_test(recipe_ingredients, retract).

alter_test(recipe_ingredients, Pred) :-
    call(Pred, recipes_kb:sufficient("TEST_RECIPE")),
    call(Pred, recipes_kb:ingredients("TEST_RECIPE",[
        ["TEST_INGREDIENT_A","NATURAL",3],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test(recipe_ingredients, [
    setup(setup_test(recipe_ingredients)),
    cleanup(cleanup_test(recipe_ingredients)),
    nondet
]) :-
    excluded:default(Excluded),
    recipe:instance("TEST_RECIPE", Nutritions, none, none, Excluded),

    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, calories, ACals),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, proteins, AProts),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, fats, AFats),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, carbohydrates, ACarbs),

    nutritions:new(cals(ACals), prots(AProts), fats(AFats), carbs(ACarbs), IANutritions),

    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, calories, BCals),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, proteins, BProts),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, fats, BFats),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, carbohydrates, BCarbs),

    nutritions:new(cals(BCals), prots(BProts), fats(BFats), carbs(BCarbs), IBNutritions),

    nutritions:combine([IANutritions, IBNutritions], ExpectedNutritions),

    assertion(ExpectedNutritions == Nutritions).

:- end_tests(recipe_ingredients).

% ---------------------------------------------------------------------------- %
% ----------------------- TEST RECIPE INGREDIENTS ---------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++ TEST RECIPE INGREDIENTS ++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

setup_suite(recipe_variants) :-
    alter_suite(recipe_variants, assert).

cleanup_suite(recipe_variants) :-
    alter_suite(recipe_variants, retract).

alter_suite(recipe_variants, Pred) :-
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,proteins,5)).

:- begin_tests(recipe_variants, [
    setup(setup_suite(recipe_variants)),
    cleanup(cleanup_suite(recipe_variants))
]).

setup_test(recipe_variant) :-
    alter_test(recipe_variant, assert).

cleanup_test(recipe_variant) :-
    alter_test(recipe_variant, retract).

alter_test(recipe_variant, Pred) :-
    call(Pred, recipes_kb:variant(
        "TEST_RECIPE", [standalone, "VARIANT_ID"], [
            ["TEST_INGREDIENT_A","NATURAL",3],
            ["TEST_INGREDIENT_B","GRAM",200]
        ]
    )).

test(recipe_variant, [
    setup(setup_test(recipe_variant)),
    cleanup(cleanup_test(recipe_variant))
]) :-
    recipe:standalone_variant("TEST_RECIPE", "VARIANT_ID", VariantIngredients),
    recipe:ingredients_nutritions(VariantIngredients, Nutritions),

    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, calories, ACals),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, proteins, AProts),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, fats, AFats),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, carbohydrates, ACarbs),

    nutritions:new(cals(ACals), prots(AProts), fats(AFats), carbs(ACarbs), IANutritions),

    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, calories, BCals),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, proteins, BProts),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, fats, BFats),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, carbohydrates, BCarbs),

    nutritions:new(cals(BCals), prots(BProts), fats(BFats), carbs(BCarbs), IBNutritions),

    nutritions:combine([IANutritions, IBNutritions], ExpectedNutritions),

    assertion(ExpectedNutritions == Nutritions).

:- end_tests(recipe_variants).

% ---------------------------------------------------------------------------- %
% ----------------------- TEST RECIPE INGREDIENTS ---------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++ TEST RECIPE COMPLEMENTS ++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

setup_suite(recipe_complements) :-
    alter_suite(recipe_complements, assert).

cleanup_suite(recipe_complements) :-
    alter_suite(recipe_complements, retract).

alter_suite(recipe_complements, Pred) :-
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,proteins,5)).

:- begin_tests(recipe_complements, [
    setup(setup_suite(recipe_complements)),
    cleanup(cleanup_suite(recipe_complements))
]).

% ============================================================================ %

setup_test(complements) :-
    alter_test(complements, assert).

cleanup_test(complements) :-
    alter_test(complements, retract).

alter_test(complements, Pred) :-
    call(Pred, recipes_kb:complements("TEST_RECIPE", "COMPLEMENTS_ID", [
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "VARIANT_ID"]
    ])),
    call(Pred, recipes_kb:ingredients("TEST_COMPLEMENT_A", [
        ["TEST_INGREDIENT_A","NATURAL",2],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])),
    call(Pred, recipes_kb:ingredients("TEST_COMPLEMENT_B", [
        ["TEST_INGREDIENT_A","NATURAL",2]
    ])),
    call(Pred, recipes_kb:variant(
        "TEST_COMPLEMENT_B",
        [embeddable, "VARIANT_ID"],
        [["TEST_INGREDIENT_B","GRAM",200]]
    )).

test(complements, [
    setup(setup_test(complements)),
    cleanup(cleanup_test(complements)),
    nondet
]) :-
    recipes_kb:complements("TEST_RECIPE", "COMPLEMENTS_ID", Complements),
    recipe:complements_nutritions(Complements, Nutritions),

    recipes_kb:ingredients("TEST_COMPLEMENT_A", CAIngredients),
    recipe:ingredients_nutritions(CAIngredients, CAINutritions),

    nutritions:new(
        cals(400), prots(20), fats(20), carbs(100),
        ExpectedCAINutritions
    ), assertion(ExpectedCAINutritions == CAINutritions),

    recipes_kb:ingredients("TEST_COMPLEMENT_B", BIngredients),
    recipe:ingredients_nutritions(BIngredients, CBINutritions),

    nutritions:new(
        cals(200), prots(10), fats(10), carbs(50),
        ExpectedCBINutritions
    ), assertion(ExpectedCBINutritions == CBINutritions),

    recipe:embeddable_variant("TEST_COMPLEMENT_B", "VARIANT_ID", CBVIngredients),
    recipe:ingredients_nutritions(CBVIngredients, CBVINutritions),

    nutritions:new(
        cals(200), prots(10), fats(10), carbs(50),
        ExpectedCBVINutritions
    ), assertion(ExpectedCBVINutritions == CBVINutritions),

    nutritions:combine(
        [CAINutritions, CBINutritions, CBVINutritions],
        ExpectedNutritions
    ), assertion(ExpectedNutritions == Nutritions).

:- end_tests(recipe_complements).

% ---------------------------------------------------------------------------- %
% ----------------------- TEST RECIPE COMPLEMENTS ---------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++++ TEST ALLOWED RECIPE ++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(allowed_recipe).

test(empty_excluded) :-
    excluded:default(Excluded),
    recipe:allowed_recipe("TEST_RECIPE", Excluded).

% ============================================================================ %

test(not_excluded) :-
    excluded:new(recipes(["TEST_RECIPE_B"]), ingredients([]), Excluded),
    recipe:allowed_recipe("TEST_RECIPE_A", Excluded).

% ============================================================================ %

test(excluded) :-
    excluded:new(recipes(["TEST_RECIPE_A"]), ingredients([]), Excluded),
    \+ recipe:allowed_recipe("TEST_RECIPE_A", Excluded).

:- end_tests(allowed_recipe).

% ---------------------------------------------------------------------------- %
% ------------------------- TEST ALLOWED RECIPE ------------------------------ %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++ TEST ALLOWED INGREDIENTS +++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(allowed_ingredients).

test(empty_excluded) :-
    excluded:default(Excluded),
    recipe:allowed_ingredients([
        ["TEST_INGREDIENT_A","TEST_UNIT_A",2],
        ["TEST_INGREDIENT_B","TEST_UNIT_B",1]
    ], Excluded).

% ============================================================================ %

test(not_excluded) :-
    excluded:new(recipes([]), ingredients(["TEST_INGREDIENT_C"]), Excluded),
    recipe:allowed_ingredients([
        ["TEST_INGREDIENT_A","TEST_UNIT_A",2],
        ["TEST_INGREDIENT_B","TEST_UNIT_B",1]
    ], Excluded).

% ============================================================================ %

test(excluded) :-
    excluded:new(recipes([]), ingredients(["TEST_INGREDIENT_B"]), Excluded),
    \+ recipe:allowed_ingredients([
        ["TEST_INGREDIENT_A","TEST_UNIT_A",2],
        ["TEST_INGREDIENT_B","TEST_UNIT_B",1]
    ], Excluded).

:- end_tests(allowed_ingredients).

% ---------------------------------------------------------------------------- %
% ----------------------- TEST ALLOWED INGREDIENTS --------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++ TEST ALLOWED COMPLEMENTS +++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

setup_suite(allowed_complements) :-
    alter_suite(allowed_complements, assert).

cleanup_suite(allowed_complements) :-
    alter_suite(allowed_complements, retract).

alter_suite(allowed_complements, Pred) :-
    call(Pred, recipes_kb:ingredients("TEST_COMPLEMENT_A", [
        ["TEST_INGREDIENT_A","NATURAL",2],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])),
    call(Pred, recipes_kb:ingredients("TEST_COMPLEMENT_B", [
        ["TEST_INGREDIENT_A","NATURAL",2]
    ])),
    call(Pred, recipes_kb:variant(
        "TEST_COMPLEMENT_B", [embeddable, "VARIANT_ID"], [
            ["TEST_INGREDIENT_B","GRAM",200]
        ]
    )).

:- begin_tests(allowed_complements, [
    setup(setup_suite(allowed_complements)),
    cleanup(cleanup_suite(allowed_complements))
]).

% ============================================================================ %

test(empty_excluded, [nondet]) :-
    excluded:default(Excluded),
    recipe:allowed_complements([
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "VARIANT_ID"]
    ], Excluded).

% ============================================================================ %

test(not_excluded, [nondet]) :-
    Complements = [
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "VARIANT_ID"]
    ],

    excluded:new(
        recipes(["TEST_COMPLEMENT_C"]), ingredients([]), ExcludedRecipes
    ), recipe:allowed_complements(Complements, ExcludedRecipes),
    excluded:new(
        recipes([]), ingredients(["TEST_INGREDIENT_C"]), ExcludedIngredients
    ), recipe:allowed_complements(Complements, ExcludedIngredients),
    excluded:new(
        recipes(["TEST_COMPLEMENT_C"]), ingredients(["TEST_INGREDIENT_C"]),
        ExcludedComplete
    ), recipe:allowed_complements(Complements, ExcludedComplete).

% ============================================================================ %

test(excluded_recipes, [nondet]) :-
    excluded:new(
        recipes(["TEST_COMPLEMENT_B", "TEST_COMPLEMENT_C"]), ingredients([]),
        Excluded
    ),
    \+ recipe:allowed_complements([
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ], Excluded).

% ============================================================================ %

test(excluded_ingredients, [nondet]) :-
    excluded:new(
        recipes([]), ingredients(["TEST_INGREDIENT_B", "TEST_INGREDIENT_C"]),
        Excluded
    ),
    \+ recipe:allowed_complements([
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ], Excluded).

:- end_tests(allowed_complements).

% ---------------------------------------------------------------------------- %
% ----------------------- TEST ALLOWED COMPLEMENTS --------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++++++ TEST NUTRITIONS ++++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(nutritions).

test(default) :-
    nutritions:default(Nutritions),
    nutritions:cals(Nutritions, Cals), assertion(Cals == 0),
    nutritions:prots(Nutritions, Prots), assertion(Prots == 0),
    nutritions:fats(Nutritions, Fats), assertion(Fats == 0),
    nutritions:carbs(Nutritions, Carbs), assertion(Carbs == 0).

% ============================================================================ %

test(new) :-
    nutritions:new(cals(1), prots(2), fats(3), carbs(4), Nutritions),
    nutritions:cals(Nutritions, Cals), assertion(Cals == 1),
    nutritions:prots(Nutritions, Prots), assertion(Prots == 2),
    nutritions:fats(Nutritions, Fats), assertion(Fats == 3),
    nutritions:carbs(Nutritions, Carbs), assertion(Carbs == 4).

% ============================================================================ %

test(cals) :-
    nutritions:new(cals(1), prots(0), fats(0), carbs(0), Nutritions),
    nutritions:cals(Nutritions, Cals), assertion(Cals == 1).
    
% ============================================================================ %

test(prots) :-
    nutritions:new(cals(0), prots(1), fats(0), carbs(0), Nutritions),
    nutritions:prots(Nutritions, Prots), assertion(Prots == 1).

% ============================================================================ %

test(fats) :-
    nutritions:new(cals(0), prots(0), fats(1), carbs(0), Nutritions),
    nutritions:fats(Nutritions, Fats), assertion(Fats == 1).

% ============================================================================ %

test(carbs) :-
    nutritions:new(cals(0), prots(0), fats(0), carbs(1), Nutritions),
    nutritions:carbs(Nutritions, Carbs), assertion(Carbs == 1).

% ============================================================================ %

test(combine_base) :-
    nutritions:new(cals(1), prots(1), fats(1), carbs(1), ANutritions),
    nutritions:new(cals(2), prots(2), fats(2), carbs(2), BNutritions),
    nutritions:new(cals(3), prots(3), fats(3), carbs(3), CNutritions),
    nutritions:combine(ANutritions, BNutritions, CNutritions).

% ============================================================================ %

test(combine_many) :-
    nutritions:new(cals(1), prots(1), fats(1), carbs(1), ANutritions),
    nutritions:new(cals(2), prots(2), fats(2), carbs(2), BNutritions),
    nutritions:new(cals(3), prots(3), fats(3), carbs(3), CNutritions),
    nutritions:combine([ANutritions, BNutritions], CNutritions).

:- end_tests(nutritions).

% ---------------------------------------------------------------------------- %
% --------------------------- TEST NUTRITIONS -------------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++++++ TEST FORMATTERS ++++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(forematters).

test(format_nutritions) :-
    nutritions:default(Nutritions),
    menu:format_nutritions(Nutritions, Txt),

    merge_strings([
        "{",
            "\"calories\": 0, ",
            "\"proteins\": 0, ",
            "\"fats\": 0, ",
            "\"carbohydrates\": 0",
        "}"
    ], ExpectedTxt), assertion(ExpectedTxt == Txt).

% ============================================================================ %

test(format_recipe_base, [nondet]) :-
    nutritions:default(Nutritions),
    menu:format_recipe(["TEST_RECIPE", Nutritions, none, none], Txt),

    merge_strings([
        "{",
            "\"recipe\": \"TEST_RECIPE\", ",
            "\"variant\": null, ",
            "\"complements\": null",
        "}"
    ], ExpectedTxt), assertion(ExpectedTxt == Txt).
    
% ============================================================================ %

test(format_recipe_with_variant, [nondet]) :-
    nutritions:default(Nutritions),
    menu:format_recipe(["TEST_RECIPE", Nutritions, "INGREDIENTS_ID", none], Txt),

    merge_strings([
        "{",
            "\"recipe\": \"TEST_RECIPE\", ",
            "\"variant\": \"INGREDIENTS_ID\", ",
            "\"complements\": null",
        "}"
    ], ExpectedTxt), assertion(ExpectedTxt == Txt).

% ============================================================================ %

test(format_recipe_with_complements, [nondet]) :-
    nutritions:default(Nutritions),
    menu:format_recipe(["TEST_RECIPE", Nutritions, none, "COMPLEMENTS_ID"], Txt),

    merge_strings([
        "{",
            "\"recipe\": \"TEST_RECIPE\", ",
            "\"variant\": null, ",
            "\"complements\": \"COMPLEMENTS_ID\"",
        "}"
    ], ExpectedTxt), assertion(ExpectedTxt == Txt).

% ============================================================================ %

test(format_recipe_complete, [nondet]) :-
    nutritions:default(Nutritions),
    menu:format_recipe(["TEST_RECIPE", Nutritions, "INGREDIENTS_ID", "COMPLEMENTS_ID"], Txt),

    merge_strings([
        "{",
            "\"recipe\": \"TEST_RECIPE\", ",
            "\"variant\": \"INGREDIENTS_ID\", ",
            "\"complements\": \"COMPLEMENTS_ID\"",
        "}"
    ], ExpectedTxt), assertion(ExpectedTxt == Txt).

:- end_tests(forematters).

% ---------------------------------------------------------------------------- %
% --------------------------- TEST FORMATTERS -------------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++++ TEST RCIPE INSTANCE ++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

setup_suite(recipe_instance) :-
    alter_suite(recipe_instance, assert).

cleanup_suite(recipe_instance) :-
    alter_suite(recipe_instance, retract).

alter_suite(recipe_instance, Pred) :-
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","NATURAL",1,calories,100)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","NATURAL",1,carbohydrates,25)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","NATURAL",1,fats,5)),
    call(Pred, ingredients_kb:nutrition("TEST_INGREDIENT_B","NATURAL",1,proteins,5)),

    call(Pred, recipes_kb:ingredients("TEST_RECIPE",[
        ["TEST_INGREDIENT_A","NATURAL",1]
    ])),
    call(Pred, recipes_kb:ingredients("TEST_COMPLEMENT_A",[
        ["TEST_INGREDIENT_A","NATURAL",1]
    ])),
    call(Pred, recipes_kb:ingredients("TEST_COMPLEMENT_B",[
        ["TEST_INGREDIENT_A","NATURAL",1]
    ])),

    call(Pred, recipes_kb:variant(
        "TEST_COMPLEMENT_B", [embeddable, "TEST_COMPLEMENT_B_VARIANT_ID"], [
            ["TEST_INGREDIENT_B","NATURAL",1]
        ]
    )).


:- begin_tests(recipe_instance, [
    setup(setup_suite(recipe_instance)),
    cleanup(cleanup_suite(recipe_instance))
]).

:- discontiguous plunit_recipe_instance:setup_test/1.
:- discontiguous plunit_recipe_instance:cleanup_test/1.
:- discontiguous plunit_recipe_instance:alter_test/2.

% ============================================================================ %

test(recipe_instance_base) :-
    \+ recipe:instance("TEST_RECIPE", _Nutritions, none, none, [[], []]).

% ============================================================================ %

setup_test(recipe_instance_base) :-
    alter_test(recipe_instance_base, assert).

cleanup_test(recipe_instance_base) :-
    alter_test(recipe_instance_base, retract).

alter_test(recipe_instance_base, Pred) :-
    call(Pred, recipes_kb:sufficient("TEST_RECIPE")).

test(recipe_instance_base_sufficient, [
    setup(setup_test(recipe_instance_base)),
    cleanup(cleanup_test(recipe_instance_base)),
    nondet
]) :-
    excluded:default(Excluded),
    recipe:instance("TEST_RECIPE", Nutritions, none, none, Excluded),
    nutritions:new(
        cals(100), prots(5), fats(5), carbs(25),
        ExpectedNutritions
    ), assertion(ExpectedNutritions == Nutritions).

% ============================================================================ %

setup_test(recipe_instance_variant) :-
    alter_test(recipe_instance_variant, assert).

cleanup_test(recipe_instance_variant) :-
    alter_test(recipe_instance_variant, retract).

alter_test(recipe_instance_variant, Pred) :-
    call(Pred, recipes_kb:variant(
        "TEST_RECIPE", [standalone, "VARIANT_ID"], [
            ["TEST_INGREDIENT_B","NATURAL",1]
        ]
    )).

test(recipe_instance_variant, [
    setup(setup_test(recipe_instance_variant)),
    cleanup(cleanup_test(recipe_instance_variant)),
    nondet
]) :-
    excluded:default(Excluded),
    recipe:instance("TEST_RECIPE", Nutritions, "VARIANT_ID", none, Excluded),
    nutritions:new(
        cals(200), prots(10), fats(10), carbs(50),
        ExpectedNutritions
    ), assertion(ExpectedNutritions == Nutritions).

% ============================================================================ %

setup_test(recipe_instance_complements) :-
    alter_test(recipe_instance_complements, assert).

cleanup_test(recipe_instance_complements) :-
    alter_test(recipe_instance_complements, retract).

alter_test(recipe_instance_complements, Pred) :-
    call(Pred, recipes_kb:complements("TEST_RECIPE","COMPLEMENTS_ID",[
        ["TEST_COMPLEMENT_A",none],
        ["TEST_COMPLEMENT_B","TEST_COMPLEMENT_B_INGREDIENTS_ID"]
    ])).

test(recipe_instance_complements, [
    setup(setup_test(recipe_instance_complements)),
    cleanup(cleanup_test(recipe_instance_complements)),
    nondet
]) :-
    \+ recipe:instance("TEST_RECIPE", _Nutritions, none, "COMPLEMENTS_ID", [[], []]).

% ============================================================================ %

setup_test(recipe_instance_complements_sufficient) :-
    alter_test(recipe_instance_complements_sufficient, assert).

cleanup_test(recipe_instance_complements_sufficient) :-
    alter_test(recipe_instance_complements_sufficient, retract).

alter_test(recipe_instance_complements_sufficient, Pred) :-
    call(Pred, recipes_kb:sufficient("TEST_RECIPE")),
    call(Pred, recipes_kb:complements("TEST_RECIPE","COMPLEMENTS_ID",[
        ["TEST_COMPLEMENT_A",none],
        ["TEST_COMPLEMENT_B","TEST_COMPLEMENT_B_VARIANT_ID"]
    ])).

test(recipe_instance_complements_sufficient, [
    setup(setup_test(recipe_instance_complements_sufficient)),
    cleanup(cleanup_test(recipe_instance_complements_sufficient)),
    nondet
]) :-
    excluded:default(Excluded),
    recipe:instance("TEST_RECIPE", Nutritions, none, "COMPLEMENTS_ID", Excluded),
    nutritions:new(
        cals(400), prots(20), fats(20), carbs(100),
        ExpectedNutritions
    ), assertion(ExpectedNutritions == Nutritions).

% ============================================================================ %

setup_test(recipe_instance_complete) :-
    alter_test(recipe_instance_complete, assert).

cleanup_test(recipe_instance_complete) :-
    alter_test(recipe_instance_complete, retract).

alter_test(recipe_instance_complete, Pred) :-
    call(Pred, recipes_kb:variant(
        "TEST_RECIPE", [standalone, "VARIANT_ID"], [
            ["TEST_INGREDIENT_B","NATURAL",1]
        ]
    )),
    call(Pred, recipes_kb:complements("TEST_RECIPE","COMPLEMENTS_ID",[
        ["TEST_COMPLEMENT_A",none],
        ["TEST_COMPLEMENT_B","TEST_COMPLEMENT_B_VARIANT_ID"]
    ])).

test(recipe_instance_complete, [
    setup(setup_test(recipe_instance_complete)),
    cleanup(cleanup_test(recipe_instance_complete)),
    nondet
]) :-
    excluded:default(Excluded),
    recipe:instance(
        "TEST_RECIPE", Nutritions, "VARIANT_ID", "COMPLEMENTS_ID", Excluded
    ),
    nutritions:new(
        cals(500), prots(25), fats(25), carbs(125),
        ExpectedNutritions
    ), assertion(ExpectedNutritions == Nutritions).

:- end_tests(recipe_instance).

% ---------------------------------------------------------------------------- %
% ------------------------- TEST RCIPE INSTANCE ------------------------------ %
% ---------------------------------------------------------------------------- %


% ============================================================================ %
% ================================ UTILS ===================================== %
% ============================================================================ %

merge_strings(Strings, Txt) :-
    lists:reverse(Strings, Rev), foldl(string_concat, Rev, "", Txt).

% ============================================================================ %
% ============================================================================ %
% ============================================================================ %
