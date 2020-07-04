from lib.validators.util import validate_schema, validate_code


def validate(ingredients, codes, schemas):
    validate_ingredients(ingredients['regular'], schemas['regular_ingredient'], codes)
    validate_ingredients(ingredients['technical'], schemas['technical_ingredient'], codes)


def validate_ingredients(ingredients, schema, codes):
    for ingredient_name, ingredient in ingredients.items():
        validate_code(ingredient_name, codes)
        validate_schema(ingredient, schema)
