:- begin_tests(recipe_meal).


test(breakfast, [
    setup(assert(recipes_kb:meal("TEST_RECIPE", "BREAKFAST"))),
    cleanup(retract(recipes_kb:meal("TEST_RECIPE", "BREAKFAST")))
]) :-
    recipe:breakfast("TEST_RECIPE").


test(snack, [
    setup(assert(recipes_kb:meal("TEST_RECIPE", "SNACK"))),
    cleanup(retract(recipes_kb:meal("TEST_RECIPE", "SNACK")))
]) :-
    recipe:snack("TEST_RECIPE").


:- end_tests(recipe_meal).


:- begin_tests(ingredient_nutrition).

:- discontiguous plunit_ingredient_nutrition:setup/1.
:- discontiguous plunit_ingredient_nutrition:cleanup/1.

setup(single_unit) :-
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,proteins,5)).

cleanup(single_unit) :-
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT",1,proteins,5)).

test(single_unit, [
    setup(setup(single_unit)),
    cleanup(cleanup(single_unit))
]) :-
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,calories,500),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,carbohydrates,125),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,fats,25),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT",5,proteins,25).


setup(multiple_units) :-
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,proteins,5)),

    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,calories,100)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,carbohydrates,25)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,fats,5)),
    assert(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,proteins,5)).

cleanup(multiple_units) :-
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_A",1,proteins,5)),

    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,calories,100)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,carbohydrates,25)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,fats,5)),
    retract(ingredients_kb:nutrition("TEST_INGREDIENT","TEST_UNIT_B",100,proteins,5)).

test(multiple_units, [
    setup(setup(multiple_units)),
    cleanup(cleanup(multiple_units)),
    nondet
]) :-
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,calories,500),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,carbohydrates,125),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,fats,25),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_A",5,proteins,25),

    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,calories,500),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,carbohydrates,125),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,fats,25),
    ingredient:nutrition_query("TEST_INGREDIENT","TEST_UNIT_B",500,proteins,25).


:- end_tests(ingredient_nutrition).
