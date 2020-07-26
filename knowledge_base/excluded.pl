:- module(excluded, [
    default/1, new/3,
    recipes/2, ingredients/2
]).

:- use_module(library(record)).

:- record excluded(recipes=[], ingredients=[]).

default(E) :-
    default_excluded(E).

new(recipes(Recipes), ingredients(Ingredients), E) :-
    make_excluded([recipes(Recipes), ingredients(Ingredients)], E).

recipes(E, Val) :- excluded_recipes(E, Val).
ingredients(E, Val) :- excluded_ingredients(E, Val).
