:- module(ingredient, [
    characteristic_query/5
]).

:- use_module(ingredients_kb, [
    characteristic/5
]).

characteristic_query(
    Ingredient, Characteristic, Unit, Quantity, Value
) :-
    ingredients_kb:characteristic(
        Ingredient, Characteristic, Unit, BaseQuantity, BaseValue
    ),
    Value is Quantity / BaseQuantity * BaseValue.
