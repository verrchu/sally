from lib.validators.util import validate_schema

def validate(recipes, ingredients, schemas):
    schema = schemas['recipe']

    for recipe_name, recipe in recipes.items():
        validate_schema(recipe, schema)
