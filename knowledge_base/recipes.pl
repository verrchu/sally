:- module(recipes, [
    recipe_type/2,
    recipe_ingredients/2
]).

recipe_type('CHEESE_PANCAKE','BREAKFAST').
recipe_type('OAT_PANCAKE','BREAKFAST').
recipe_type('OMELET','BREAKFAST').

recipe_ingredients('CHEESE_PANCAKE',[
    ['COTTAGE_CHEESE','GRAM',220],
    ['EGG','NATURAL',1],
    ['FLOUR','TABLESPOON',1]
]).
recipe_ingredients('OAT_PANCAKE',[
    ['EGG','NATURAL',1],
    ['GLAIR','NATURAL',2],
    ['OAT','GRAM',40]
]).
recipe_ingredients('OMELET',[
    ['EGG','NATURAL',2],
    ['FLOUR','TABLESPOON',1],
    ['GLAIR','NATURAL',1],
    ['MILK','MILLILITER',100]
]).

