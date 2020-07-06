from lib.validators.util import validate_schema

def validate(recipes, ingredients, schemas):
    schema = schemas['recipe']

    for recipe_name, recipe in recipes.items():
        validate_schema(recipe, schema)
        validate_recipe_ingredients(
            recipe_name, recipe['ingredients'],  ingredients
        )


def validate_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    validate_main_recipe_ingredients(
        recipe_name, recipe_ingredients['main'], ingredients['regular']
    )
    if 'additional' in recipe_ingredients:
        validate_additional_recipe_ingredients(
            recipe_name, recipe_ingredients['additional'], ingredients['regular']
        )
    if 'technical' in recipe_ingredients:
        validate_technical_recipe_ingredients(
            recipe_name, recipe_ingredients['technical'], ingredients['technical']
        )


def validate_main_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    for ingredient_name, ingredient in recipe_ingredients.items():
        if not ingredient_name in ingredients:
            raise Exception(f"""
            UNKNOWN INGREDIENT USED
            RECIPE: {recipe_name}
            INGREDIENT: {ingredient_name}
            """)


def validate_additional_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    pass


def validate_technical_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    pass
