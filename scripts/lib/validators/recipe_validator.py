from lib.validators.util import validate_schema

def validate(recipes, ingredients, schemas):
    schema = schemas['recipe']

    for recipe_name, recipe in recipes.items():
        validate_schema(recipe, schema)
        validate_ingredients(
            recipe_name, recipe['ingredients'],  ingredients
        )
        if 'variants' in recipe:
            validate_variants(
                recipe_name, recipe['variants'], recipe['ingredients'], ingredients
            )
        if 'complements' in recipe:
            validate_complements(
                recipe_name, recipe['complements'], recipes, ingredients
            )
        validate_flags(recipe_name, recipe)


def validate_flags(recipe_name, recipe):
    if not 'meals' in recipe and not recipe['embeddable']:
        raise Exception(f"""
        RECIPE IN NEITHER STANDALONE NOR EMBEDDABLE
        RECIPE: {recipe_name}
        """)

    if 'variants' in recipe:
        for variant_id, variant in recipe['variants'].items():
            if not variant['embeddable'] and not variant['standalone']:
                raise Exception(f"""
                RECIPE VARIANT IS NEITHER EMBEDDABLE NOR STANDALONE
                RECIPE: {recipe_name}
                VARIANT: {variant_id}
                """)

            if variant['embeddable']:
                if not recipe['embeddable']:
                    raise Exception(f"""
                    NOT EMBEDDABLE RECIPE HAS EMBEDDABLE VARIANT
                    RECIPE: {recipe_name}
                    VARIANT: {variant_id}
                    """)

            if variant['standalone']:
                if not 'meals' in recipe:
                    raise Exception(f"""
                    NOT STANDALONE RECIPE HAS STANDLAONE VARIANT
                    RECIPE: {recipe_name}
                    VARIANT: {variant_id}
                    """)
    else:
        if not recipe['sufficient']:
            raise Exception(f"""
            INSUFFICIENT RECIPE HAS NO VARIANTS
            RECIPE: {recipe_name}
            """)


def validate_ingredients(recipe_name, recipe_ingredients, ingredients):
    validate_regular_ingredients(
        recipe_name, recipe_ingredients['regular'], ingredients['regular']
    )
    if 'technical' in recipe_ingredients:
        validate_technical_ingredients(
            recipe_name, recipe_ingredients['technical'], ingredients['technical']
        )


def validate_variants(
    recipe_name, recipe_variants, recipe_ingredients, ingredients
):
    flattened_variants = []
    for variant in recipe_variants.values():
        for ingredient_name, ingredient in variant['ingredients'].items():
            flattened_variants.append((ingredient_name, ingredient))

    for ingredient_name, ingredient in flattened_variants:
        validate_regular_ingredient(
            recipe_name, ingredient_name, ingredient, ingredients['regular']
        )

    regular_ingredient_names = set(recipe_ingredients['regular'].keys())
    variant_ingredient_names = set()
    for ingredient_name, ingredient in flattened_variants:
        variant_ingredient_names.add(ingredient_name)

    intersection = list(
        regular_ingredient_names.intersection(variant_ingredient_names)
    )
    if len(intersection):
        raise Exception(f"""
        SAME INGREDIENTS USED AS BOTH MAIN AND ADDITIONAL
        RECIPE: {recipe_name}
        INGREDIENTS: {intersection}
        """)


def validate_complements(
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

            if complement['variant']:
                if 'variants' in complement_recipe:
                    complement_variant_id = complement['variant']
                    defined_variants = list(
                        complement_recipe['variants'].keys()
                    )

                    if not complement_variant_id in defined_variants:
                        raise Exception(f"""
                        COMPLEMENT VARIANT NOT FOUND
                        RECIPE: {recipe_name}
                        COMPLEMENT: {complement_name}
                        COMPLEMENTS iD: {complements_id}
                        VARIANT: {complement_variant_id}
                        DEFINED VARIANTS: {defined_variants}
                        """)

                    complement_variant = (
                        complement_recipe['variants'][complement_variant_id]
                    )

                    if not complement_variant['embeddable']:
                        raise Exception(f"""
                        COMPLEMENT VARIANT NOT EMBEDDABLE
                        RECIPE: {recipe_name}
                        COMPLEMENT: {complement_name}
                        COMPLEMENTS iD: {complements_id}
                        VARIANT: {complement_variant_id}
                        """)
                else:
                    raise Exception(f"""
                    VARIANTS NOT DEFINED
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


def validate_regular_ingredients(recipe_name, recipe_ingredients, ingredients):
    for ingredient_name, ingredient in recipe_ingredients.items():
        validate_regular_ingredient(
            recipe_name, ingredient_name, ingredient, ingredients
        )


def validate_technical_ingredients(recipe_name, recipe_ingredients, ingredients):
    for ingredient_name, ingredient in recipe_ingredients.items():
        validate_technical_ingredient(
            recipe_name, ingredient_name, ingredient, ingredients
        )


def validate_regular_ingredient(
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


def validate_technical_ingredient(
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
