:- module(recipes_kb, [
    type/2,
    ingredients/2
]).

type('CHEESE_PANCAKE','BREAKFAST').
type('OAT_PANCAKE','BREAKFAST').
type('OMELET','BREAKFAST').

ingredients('CHEESE_PANCAKE',[
    ['COTTAGE_CHEESE','GRAM',220],
    ['EGG','NATURAL',1],
    ['FLOUR','TABLESPOON',1]
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

