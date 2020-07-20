:- begin_tests(recipe_meal).


test(breakfast, [
    setup(assert(recipes_kb:meal("TEST", "BREAKFAST"))),
    cleanup(retract(recipes_kb:meal("TEST", "BREAKFAST")))
]) :-
    recipe:breakfast("TEST").


test(snack, [
    setup(assert(recipes_kb:meal("TEST", "SNACK"))),
    cleanup(retract(recipes_kb:meal("TEST", "SNACK")))
]) :-
    recipe:snack("TEST").


:- end_tests(recipe_meal).
