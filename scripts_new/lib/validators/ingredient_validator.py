from lib.validators.util import validate_schema


def validate(ingredients, schemas):
    validate_ingredients(ingredients['regular'], schemas['regular_ingredient'])
    validate_ingredients(ingredients['technical'], schemas['technical_ingredient'])


def validate_ingredients(ingredients, schema):
    for ingredient_name, ingredient in ingredients.items():
        validate_schema(ingredient, schema)
