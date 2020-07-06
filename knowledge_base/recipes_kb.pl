
:- module(recipes_kb, [
    meal/2,
    main_ingredients/2,
    additional_ingredients/2
]).

meal('OMELET','BREAKFAST').
meal('CHEESE_PANCAKE','BREAKFAST').
meal('BANANA_PANCAKE','BREAKFAST').
meal('FRIED_EGGS','BREAKFAST').
meal('OAT_PANCAKE','BREAKFAST').

main_ingredients('OMELET',[
    ['EGG','NATURAL',2],
    ['GLAIR','NATURAL',1],
    ['MILK','MILLILITER',100],
    ['FLOUR','TABLESPOON',1]
]).
main_ingredients('CHEESE_PANCAKE',[
    ['EGG','NATURAL',1],
    ['FLOUR','TABLESPOON',1],
    ['COTTAGE_CHEESE','GRAM',220]
]).
main_ingredients('BANANA_PANCAKE',[
    ['EGG','NATURAL',1],
    ['BANANA','NATURAL',1],
    ['OAT','GRAM',15]
]).
main_ingredients('FRIED_EGGS',[
    ['EGG','NATURAL',2]
]).
main_ingredients('OAT_PANCAKE',[
    ['EGG','NATURAL',1],
    ['GLAIR','NATURAL',2],
    ['OAT','GRAM',40]
]).

additional_ingredients('OMELET',[]).
additional_ingredients('CHEESE_PANCAKE',[]).
additional_ingredients('BANANA_PANCAKE',[
    [
        ['DARK_CHOCOLATE','GRAM',10],
        ['PEACH','NATURAL',0.5]
    ],
    [
        ['DARK_CHOCOLATE','GRAM',15]
    ]
]).
additional_ingredients('FRIED_EGGS',[]).
additional_ingredients('OAT_PANCAKE',[]).
