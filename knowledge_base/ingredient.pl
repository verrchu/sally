:- module(ingredient, [
    nutrition_query/5
]).

:- use_module(ingredients_kb, [
    nutrition/5
]).

nutrition_query(
    Ingredient, Unit, Quantity, Nutrition, Value
) :-
    ingredients_kb:nutrition(
        Ingredient, Unit, BaseQuantity, Nutrition, BaseValue
    ),
    Value is Quantity / BaseQuantity * BaseValue.
