:- discontiguous plunit:suite_setup/1.
:- discontiguous plunit:suite_cleanup/1.


% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++++++ TEST RECIPE MEAL +++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(recipe_meal).

:- discontiguous plunit_recipe_meal:test_setup/1.
:- discontiguous plunit_recipe_meal:test_cleanup/1.

% ============================================================================ %

test_setup(breakfast) :-
    assert(recipes_kb:meal("TEST_RECIPE", "BREAKFAST")).

test_cleanup(breakfast) :-
    retract(recipes_kb:meal("TEST_RECIPE", "BREAKFAST")).

test(breakfast, [
    setup(test_setup(breakfast)),
    cleanup(test_cleanup(breakfast))
]) :-
    recipe:breakfast("TEST_RECIPE").

% ============================================================================ %

test_setup(snack) :-
    assert(recipes_kb:meal("TEST_RECIPE", "SNACK")).

test_cleanup(snack) :-
    retract(recipes_kb:meal("TEST_RECIPE", "SNACK")).

test(snack, [
    setup(test_setup(snack)),
    cleanup(test_cleanup(snack))
]) :-
    recipe:snack("TEST_RECIPE").

:- end_tests(recipe_meal).

% ---------------------------------------------------------------------------- %
% --------------------------- TEST RECIPE MEAL ------------------------------- %
% ---------------------------------------------------------------------------- %


% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% ++++++++++++++++++++ TEST INGREDIENT NUTRITION QUERY +++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(ingredient_nutrition_query).

:- discontiguous plunit_ingredient_nutrition_query:test_setup/1.
:- discontiguous plunit_ingredient_nutrition_query:test_cleanup/1.

% ============================================================================ %

test_setup(single_unit) :-
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,proteins,5)).

test_cleanup(single_unit) :-
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,proteins,5)).

test(single_unit, [
    setup(test_setup(single_unit)),
    cleanup(test_cleanup(single_unit))
]) :-
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,calories,Cals), assertion(Cals == 500),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,carbohydrates,Carbs), assertion(Carbs == 125),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,fats,Fats), assertion(Fats == 25),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,proteins,Prots), assertion(Prots == 25).

% ============================================================================ %

test_setup(multiple_units) :-
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,proteins,5)),

    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,proteins,5)).

test_cleanup(multiple_units) :-
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,proteins,5)),

    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,proteins,5)).

test(multiple_units, [
    setup(test_setup(multiple_units)),
    cleanup(test_cleanup(multiple_units)),
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

suite_setup(recipe_ingredients) :-
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,proteins,5)).

suite_cleanup(recipe_ingredients) :-
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,proteins,5)).

:- begin_tests(recipe_ingredients, [
    setup(suite_setup(recipe_ingredients)),
    cleanup(suite_cleanup(recipe_ingredients))
]).

:- discontiguous plunit_recipe_ingredients:test_setup/1.
:- discontiguous plunit_recipe_ingredients:test_cleanup/1.

% ============================================================================ %

test_setup(main_ingredients) :-
    assert(recipes_kb:main_ingredients("TEST_RECIPE",[
        ["TEST_INGREDIENT_A","NATURAL",3],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test_cleanup(main_ingredients) :-
    retract(recipes_kb:main_ingredients("TEST_RECIPE",[
        ["TEST_INGREDIENT_A","NATURAL",3],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test(main_ingredients, [
    setup(test_setup(main_ingredients)),
    cleanup(test_cleanup(main_ingredients))
]) :-
    recipes_kb:main_ingredients("TEST_RECIPE", MainIngredients),
    recipe:ingredients_nutritions(MainIngredients, Nutritions),

    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, calories, ACals),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, proteins, AProts),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, fats, AFats),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, carbohydrates, ACarbs),

    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, calories, BCals),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, proteins, BProts),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, fats, BFats),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, carbohydrates, BCarbs),

    Cals is ACals + BCals,
    Prots is AProts + BProts,
    Fats is AFats + BFats,
    Carbs is ACarbs + BCarbs,

    assertion([Cals, Prots, Fats, Carbs] == Nutritions).

% ============================================================================ %

test_setup(additional_ingredients) :-
    assert(recipes_kb:additional_ingredients("TEST_RECIPE", "ID", [
        ["TEST_INGREDIENT_A","NATURAL",3],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test_cleanup(additional_ingredients) :-
    retract(recipes_kb:additional_ingredients("TEST_RECIPE", "ID", [
        ["TEST_INGREDIENT_A","NATURAL",3],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test(additional_ingredients, [
    setup(test_setup(additional_ingredients)),
    cleanup(test_cleanup(additional_ingredients))
]) :-
    recipes_kb:additional_ingredients("TEST_RECIPE", "ID", AdditionalIngredients),
    recipe:ingredients_nutritions(AdditionalIngredients, Nutritions),

    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, calories, ACals),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, proteins, AProts),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, fats, AFats),
    ingredient:nutrition_query("TEST_INGREDIENT_A", "NATURAL", 3, carbohydrates, ACarbs),

    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, calories, BCals),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, proteins, BProts),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, fats, BFats),
    ingredient:nutrition_query("TEST_INGREDIENT_B", "GRAM", 200, carbohydrates, BCarbs),

    Cals is ACals + BCals,
    Prots is AProts + BProts,
    Fats is AFats + BFats,
    Carbs is ACarbs + BCarbs,

    assertion([Cals, Prots, Fats, Carbs] == Nutritions).

:- end_tests(recipe_ingredients).

% ---------------------------------------------------------------------------- %
% ----------------------- TEST RECIPE INGREDIENTS ---------------------------- %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++ TEST RECIPE COMPLEMENTS ++++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

suite_setup(recipe_complements) :-
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,proteins,5)).

suite_cleanup(recipe_complements) :-
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_A","NATURAL",1,proteins,5)),

    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT_B","GRAM",100,proteins,5)).

:- begin_tests(recipe_complements, [
    setup(suite_setup(recipe_complements)),
    cleanup(suite_cleanup(recipe_complements))
]).

% ============================================================================ %

test_setup(complements) :-
    assert(recipes_kb:complements("TEST_RECIPE", "COMPLEMENTS_ID", [
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ])),
    assert(recipes_kb:main_ingredients("TEST_COMPLEMENT_A", [
        ["TEST_INGREDIENT_A","NATURAL",2],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])),
    assert(recipes_kb:main_ingredients("TEST_COMPLEMENT_B", [
        ["TEST_INGREDIENT_A","NATURAL",2]
    ])),
    assert(recipes_kb:additional_ingredients("TEST_COMPLEMENT_B", "INGREDIENTS_ID", [
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test_cleanup(complements) :-
    retract(recipes_kb:complements("TEST_RECIPE", "COMPLEMENTS_ID", [
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ])),
    retract(recipes_kb:main_ingredients("TEST_COMPLEMENT_A", [
        ["TEST_INGREDIENT_A","NATURAL",2],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])),
    retract(recipes_kb:main_ingredients("TEST_COMPLEMENT_B", [
        ["TEST_INGREDIENT_A","NATURAL",2]
    ])),
    retract(recipes_kb:additional_ingredients("TEST_COMPLEMENT_B", "INGREDIENTS_ID", [
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test(complements, [
    setup(test_setup(complements)),
    cleanup(test_cleanup(complements)),
    nondet
]) :-
    recipes_kb:complements("TEST_RECIPE", "COMPLEMENTS_ID", Complements),
    recipe:complements_nutritions(Complements, Nutritions),

    recipes_kb:main_ingredients("TEST_COMPLEMENT_A", AMainIngredients),
    recipe:ingredients_nutritions(AMainIngredients, [AMCals, AMProts, AMFats, AMCarbs]),

    AMNutritions = [AMCals, AMProts, AMFats, AMCarbs],
    assertion([400, 20, 20, 100] == AMNutritions),

    recipes_kb:main_ingredients("TEST_COMPLEMENT_B", BMainIngredients),
    recipe:ingredients_nutritions(BMainIngredients, [BMCals, BMProts, BMFats, BMCarbs]),

    BMNutritions = [BMCals, BMProts, BMFats, BMCarbs],
    assertion([200, 10, 10, 50] == BMNutritions),

    recipes_kb:additional_ingredients("TEST_COMPLEMENT_B", "INGREDIENTS_ID", BAdditionalIngredients),
    recipe:ingredients_nutritions(BAdditionalIngredients, [BACals, BAProts, BAFats, BACarbs]),

    BANutritions = [BACals, BAProts, BAFats, BACarbs],
    assertion([200, 10, 10, 50] == BANutritions),

    Cals is AMCals + BMCals+ BACals,
    Prots is AMProts + BMProts+ BAProts,
    Fats is AMFats + BMFats+ BAFats,
    Carbs is AMCarbs + BMCarbs+ BACarbs,

    assertion([Cals, Prots, Fats, Carbs] == Nutritions).

:- end_tests(recipe_complements).

% ---------------------------------------------------------------------------- %
% ----------------------- TEST RECIPE COMPLEMENTS ---------------------------- %
% ---------------------------------------------------------------------------- %
