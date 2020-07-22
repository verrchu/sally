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

:- discontiguous plunit_recipe_ingredients:setup_test/1.
:- discontiguous plunit_recipe_ingredients:cleanup_test/1.
:- discontiguous plunit_recipe_ingredients:alter_test/2.

% ============================================================================ %

setup_test(main_ingredients) :-
    alter_test(main_ingredients, assert).

cleanup_test(main_ingredients) :-
    alter_test(main_ingredients, retract).

alter_test(main_ingredients, Pred) :-
    call(Pred, recipes_kb:main_ingredients("TEST_RECIPE",[
        ["TEST_INGREDIENT_A","NATURAL",3],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test(main_ingredients, [
    setup(setup_test(main_ingredients)),
    cleanup(cleanup_test(main_ingredients))
]) :-
    recipes_kb:main_ingredients("TEST_RECIPE", MainIngredients),
    recipe:ingredients_nutritions(MainIngredients, Nutritions),

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

% ============================================================================ %

setup_test(additional_ingredients) :-
    alter_test(additional_ingredients, assert).

cleanup_test(additional_ingredients) :-
    alter_test(additional_ingredients, retract).

alter_test(additional_ingredients, Pred) :-
    call(Pred, recipes_kb:additional_ingredients("TEST_RECIPE", "ID", [
        ["TEST_INGREDIENT_A","NATURAL",3],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test(additional_ingredients, [
    setup(setup_test(additional_ingredients)),
    cleanup(cleanup_test(additional_ingredients))
]) :-
    recipes_kb:additional_ingredients("TEST_RECIPE", "ID", AdditionalIngredients),
    recipe:ingredients_nutritions(AdditionalIngredients, Nutritions),

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
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ])),
    call(Pred, recipes_kb:main_ingredients("TEST_COMPLEMENT_A", [
        ["TEST_INGREDIENT_A","NATURAL",2],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])),
    call(Pred, recipes_kb:main_ingredients("TEST_COMPLEMENT_B", [
        ["TEST_INGREDIENT_A","NATURAL",2]
    ])),
    call(Pred, recipes_kb:additional_ingredients("TEST_COMPLEMENT_B", "INGREDIENTS_ID", [
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

test(complements, [
    setup(setup_test(complements)),
    cleanup(cleanup_test(complements)),
    nondet
]) :-
    recipes_kb:complements("TEST_RECIPE", "COMPLEMENTS_ID", Complements),
    recipe:complements_nutritions(Complements, Nutritions),

    recipes_kb:main_ingredients("TEST_COMPLEMENT_A", AMainIngredients),
    recipe:ingredients_nutritions(AMainIngredients, AMNutritions),

    nutritions:new(
        cals(400), prots(20), fats(20), carbs(100),
        ExpectedAMNutritions
    ), assertion(ExpectedAMNutritions == AMNutritions),

    recipes_kb:main_ingredients("TEST_COMPLEMENT_B", BMainIngredients),
    recipe:ingredients_nutritions(BMainIngredients, BMNutritions),

    nutritions:new(
        cals(200), prots(10), fats(10), carbs(50),
        ExpectedBMNutritions
    ), assertion(ExpectedBMNutritions == BMNutritions),

    recipes_kb:additional_ingredients("TEST_COMPLEMENT_B", "INGREDIENTS_ID", BAdditionalIngredients),
    recipe:ingredients_nutritions(BAdditionalIngredients, BANutritions),

    nutritions:new(
        cals(200), prots(10), fats(10), carbs(50),
        ExpectedBANutritions
    ), assertion(ExpectedBANutritions == BANutritions),

    nutritions:combine(
        [AMNutritions, BMNutritions, BANutritions],
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
    recipe:allowed_recipe("TEST_RECIPE", [[], []]).

test(not_excluded) :-
    recipe:allowed_recipe("TEST_RECIPE_A", [["TEST_RECIPE_B"], []]).

test(excluded) :-
    \+ recipe:allowed_recipe("TEST_RECIPE_A", [
        ["TEST_RECIPE_A", "TEST_RECIPE_B"], []
    ]).

:- end_tests(allowed_recipe).

% ---------------------------------------------------------------------------- %
% ------------------------- TEST ALLOWED RECIPE ------------------------------ %
% ---------------------------------------------------------------------------- %

% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %
% +++++++++++++++++++++++ TEST ALLOWED INGREDIENTS +++++++++++++++++++++++++++ %
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ %

:- begin_tests(allowed_ingredients).

test(empty_excluded) :-
    recipe:allowed_ingredients([
        ["TEST_INGREDIENT_A","TEST_UNIT_A",2],
        ["TEST_INGREDIENT_B","TEST_UNIT_B",1]
    ], [[], []]).

test(not_excluded) :-
    recipe:allowed_ingredients([
        ["TEST_INGREDIENT_A","TEST_UNIT_A",2],
        ["TEST_INGREDIENT_B","TEST_UNIT_B",1]
    ], [[], ["TEST_INGREDIENT_C"]]).

test(excluded) :-
    \+ recipe:allowed_ingredients([
        ["TEST_INGREDIENT_A","TEST_UNIT_A",2],
        ["TEST_INGREDIENT_B","TEST_UNIT_B",1]
    ], [[], ["TEST_INGREDIENT_B"]]).

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
    call(Pred, recipes_kb:main_ingredients("TEST_COMPLEMENT_A", [
        ["TEST_INGREDIENT_A","NATURAL",2],
        ["TEST_INGREDIENT_B","GRAM",200]
    ])),
    call(Pred, recipes_kb:main_ingredients("TEST_COMPLEMENT_B", [
        ["TEST_INGREDIENT_A","NATURAL",2]
    ])),
    call(Pred, recipes_kb:additional_ingredients("TEST_COMPLEMENT_B", "INGREDIENTS_ID", [
        ["TEST_INGREDIENT_B","GRAM",200]
    ])).

:- begin_tests(allowed_complements, [
    setup(setup_suite(allowed_complements)),
    cleanup(cleanup_suite(allowed_complements))
]).

test(empty_excluded, [nondet]) :-
    recipe:allowed_complements([
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ], [[], []]).

test(not_excluded, [nondet]) :-
    Complements = [
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ],

    recipe:allowed_complements(Complements, [["TEST_COMPLEMENT_C"], []]),
    recipe:allowed_complements(Complements, [[], ["TEST_INGREDIENT_C"]]),
    recipe:allowed_complements(Complements, [
        ["TEST_COMPLEMENT_C"], ["TEST_INGREDIENT_C"]
    ]).

test(excluded_recipes, [nondet]) :-
    \+ recipe:allowed_complements([
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ], [["TEST_COMPLEMENT_B", "TEST_COMPLEMENT_C"], []]).

test(excluded_ingredients, [nondet]) :-
    \+ recipe:allowed_complements([
        ["TEST_COMPLEMENT_A", none],
        ["TEST_COMPLEMENT_B", "INGREDIENTS_ID"]
    ], [[], ["TEST_INGREDIENT_B", "TEST_INGREDIENT_C"]]).

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

test(new) :-
    nutritions:new(cals(1), prots(2), fats(3), carbs(4), Nutritions),
    nutritions:cals(Nutritions, Cals), assertion(Cals == 1),
    nutritions:prots(Nutritions, Prots), assertion(Prots == 2),
    nutritions:fats(Nutritions, Fats), assertion(Fats == 3),
    nutritions:carbs(Nutritions, Carbs), assertion(Carbs == 4).

test(cals) :-
    nutritions:new(cals(1), prots(0), fats(0), carbs(0), Nutritions),
    nutritions:cals(Nutritions, Cals), assertion(Cals == 1).
    
test(prots) :-
    nutritions:new(cals(0), prots(1), fats(0), carbs(0), Nutritions),
    nutritions:prots(Nutritions, Prots), assertion(Prots == 1).

test(fats) :-
    nutritions:new(cals(0), prots(0), fats(1), carbs(0), Nutritions),
    nutritions:fats(Nutritions, Fats), assertion(Fats == 1).

test(carbs) :-
    nutritions:new(cals(0), prots(0), fats(0), carbs(1), Nutritions),
    nutritions:carbs(Nutritions, Carbs), assertion(Carbs == 1).

test(combine_base) :-
    nutritions:new(cals(1), prots(1), fats(1), carbs(1), ANutritions),
    nutritions:new(cals(2), prots(2), fats(2), carbs(2), BNutritions),
    nutritions:new(cals(3), prots(3), fats(3), carbs(3), CNutritions),
    nutritions:combine(ANutritions, BNutritions, CNutritions).

test(combine_many) :-
    nutritions:new(cals(1), prots(1), fats(1), carbs(1), ANutritions),
    nutritions:new(cals(2), prots(2), fats(2), carbs(2), BNutritions),
    nutritions:new(cals(3), prots(3), fats(3), carbs(3), CNutritions),
    nutritions:combine([ANutritions, BNutritions], CNutritions).

:- end_tests(nutritions).

% ---------------------------------------------------------------------------- %
% --------------------------- TEST NUTRITIONS -------------------------------- %
% ---------------------------------------------------------------------------- %
