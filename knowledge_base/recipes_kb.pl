:- module(recipes_kb, [
    meal/2,
    ingredients/2
]).

meal('CHEESE_PANCAKE','BREAKFAST').
meal('FRIED_EGGS','BREAKFAST').
meal('OAT_PANCAKE','BREAKFAST').
meal('OMELET','BREAKFAST').

ingredients('CHEESE_PANCAKE',[
    ['COTTAGE_CHEESE','GRAM',220],
    ['EGG','NATURAL',1],
    ['FLOUR','TABLESPOON',1]
]).
ingredients('FRIED_EGGS',[
    ['EGG','NATURAL',2]
]).
ingredients('OAT_PANCAKE',[
    ['EGG','NATURAL',1],
    ['GLAIR','NATURAL',2],
    ['OAT','GRAM',40]
]).
ingredients('OMELET',[
    ['EGG','NATURAL',2],
    ['FLOUR','TABLESPOON',1],
    ['GLAIR','NATURAL',1],
    ['MILK','MILLILITER',100]
]).

