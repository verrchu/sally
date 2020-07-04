from lib.validators.util import validate_schema, validate_code

def validate(recipes, ingredients, steps, codes, schemas):
    schema = schemas['recipe']

    for recipe_name, recipe in recipes.items():
        validate_code(recipe_name, codes)
        validate_schema(recipe, schema)
