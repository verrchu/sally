from lib.validators.util import validate_schema

def validate(recipes, ingredients, schemas):
    schema = schemas['recipe']

    for recipe_name, recipe in recipes.items():
        validate_schema(recipe, schema)
        validate_recipe_ingredients(
            recipe_name, recipe['ingredients'],  ingredients
        )
        if 'complements' in recipe:
            validate_recipe_complements(
                recipe_name, recipe['complements'], recipes, ingredients
            )


def validate_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    validate_main_recipe_ingredients(
        recipe_name, recipe_ingredients['main'], ingredients['regular']
    )
    if 'additional' in recipe_ingredients:
        validate_additional_recipe_ingredients(
            recipe_name, recipe_ingredients['additional'], ingredients['regular']
        )
        main_ingredients_keys = set(recipe_ingredients['main'].keys())
        additional_ingredients_keys = set()
        for ingredients_group in recipe_ingredients['additional'].values():
            for ingredient_key in ingredients_group.keys():
                additional_ingredients_keys.add(ingredient_key)

        intersection = list(main_ingredients_keys.intersection(additional_ingredients_keys))
        if len(intersection):
            raise Exception(f"""
            SAME INGREDIENTS USED AS BOTH MAIN AND ADDITIONAL
            RECIPE: {recipe_name}
            INGREDIENTS: {intersection}
            """)
    if 'technical' in recipe_ingredients:
        validate_technical_recipe_ingredients(
            recipe_name, recipe_ingredients['technical'], ingredients['technical']
        )


def validate_main_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    for ingredient_name, ingredient in recipe_ingredients.items():
        validate_regular_recipe_ingredient(
            recipe_name, ingredient_name, ingredient, ingredients
        )


def validate_additional_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    flattened_ingredients = []
    for ingredients_group in recipe_ingredients.values():
        for ingredient_name, ingredient in ingredients_group.items():
            flattened_ingredients.append((ingredient_name, ingredient))

    for ingredient_name, ingredient in flattened_ingredients:
        validate_regular_recipe_ingredient(
            recipe_name, ingredient_name, ingredient, ingredients
        )


def validate_technical_recipe_ingredients(recipe_name, recipe_ingredients, ingredients):
    for ingredient_name, ingredient in recipe_ingredients.items():
        validate_technical_recipe_ingredient(
            recipe_name, ingredient_name, ingredient, ingredients
        )


def validate_regular_recipe_ingredient(
    recipe_name, ingredient_name, ingredient, ingredients
):
    if not ingredient_name in ingredients:
        raise Exception(f"""
        UNKNOWN INGREDIENT USED
        RECIPE: {recipe_name}
        INGREDIENT: {ingredient_name}
        """)

    ingredient_definition = ingredients[ingredient_name]
    ingredient_unit = ingredient['unit']
    defined_ingredient_units = list(ingredient_definition['units'].keys())

    if not ingredient_unit in defined_ingredient_units:
        raise Exception(f"""
        NOT SUITABLE INGREDIENT UNIT USED
        RECIPE: {recipe_name}
        INGREDIENT: {ingredient_name}
        IBGREDIENT UNIT: {ingredient_unit}
        DEFINED INGREDIENT UNITS: {defined_ingredient_units}
        """)


def validate_technical_recipe_ingredient(
    recipe_name, ingredient_name, ingredient, ingredients
):
    if not ingredient_name in ingredients:
        raise Exception(f"""
        UNKNOWN INGREDIENT USED
        RECIPE: {recipe_name}
        INGREDIENT: {ingredient_name}
        """)

    if not ingredient['quantity'] == 'ANY':
        ingredient_definition = ingredients[ingredient_name]
        ingredient_unit = ingredient['unit']
        defined_ingredient_units = ingredient_definition['units']

        if not ingredient_unit in defined_ingredient_units:
            raise Exception(f"""
            NOT SUITABLE INGREDIENT UNIT USED
            RECIPE: {recipe_name}
            INGREDIENT: {ingredient_name}
            IBGREDIENT UNIT: {ingredient_unit}
            DEFINED INGREDIENT UNITS: {defined_ingredient_units}
            """)


def validate_recipe_complements(
    recipe_name, recipe_complements, recipes, ingredients
):
    for complements_id, complements in recipe_complements.items():
        for complement_name, complement in complements.items():
            if not complement_name in recipes.keys():
                raise Exception(f"""
                UNKNOWN COMPLEMENT NAME
                RECIPE: {recipe_name}
                COMPLEMENT: {complement_name}
                COMPLEMENTS iD: {complements_id}
                """)

            complement_recipe = recipes[complement_name]

            if not complement_recipe['embeddable']:
                raise Exception(f"""
                COMPLEMENT IS NOT EMBEDDABLE
                RECIPE: {recipe_name}
                COMPLEMENT: {complement_name}
                COMPLEMENTS iD: {complements_id}
                """)

            if complement['additional_ingredients']:
                if 'additional' in complement_recipe['ingredients']:
                    additional_ingredients = complement['additional_ingredients']
                    defined_additional_ingredients = list(
                        complement_recipe['ingredients']['additional'].keys()
                    )
                    if not additional_ingredients in defined_additional_ingredients:
                        raise Exception(f"""
                        ADDITONAL INGREDIENTS ID NOT FOUND
                        RECIPE: {recipe_name}
                        COMPLEMENT: {complement_name}
                        COMPLEMENTS iD: {complements_id}
                        ADDITIONAL INGREDIENTS ID: {additional_ingredients}
                        DEFINED ADDITIONAL INGREDIENT IDS: {defined_additional_ingredients}
                        """)
                else:
                    raise Exception(f"""
                    ADDITONAL INGREDIENTS NOT DEFINED
                    RECIPE: {recipe_name}
                    COMPLEMENT: {complement_name}
                    COMPLEMENTS iD: {complements_id}
                    """)
            else:
                if not complement_recipe['sufficient']:
                    raise Exception(f"""
                    COMPLEMENT IS NOT SUFFICIENT
                    RECIPE: {recipe_name}
                    COMPLEMENT: {complement_name}
                    COMPLEMENTS iD: {complements_id}
                    """)
