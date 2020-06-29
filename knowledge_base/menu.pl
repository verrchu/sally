:- module(menu, []).
:- use_module(ingredients, [
    ingredient_characteristic/5,
    ingredient_characteristic_query/5
]).
:- use_module(recipes, [
    recipe_type/2,
    recipe_ingredients/2
]).
