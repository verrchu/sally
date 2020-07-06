import os

PREDICATE_MEAL = 'meal'
PREDICATE_MAIN_INGREDIENTS = 'main_ingredients'
PREDICATE_ADDITIONAL_INGREDIENTS = 'additional_ingredients'

MODULE_HEADER = f"""
:- module(recipes_kb, [
    {PREDICATE_MEAL}/2,
    {PREDICATE_MAIN_INGREDIENTS}/2,
    {PREDICATE_ADDITIONAL_INGREDIENTS}/2
]).
"""

KNOWLEDGE_BASE_FILE = "recipes_kb.pl"


def render(kb_dir, recipes):
    kb_path = os.path.join(kb_dir, KNOWLEDGE_BASE_FILE)

    with open(kb_path, 'w') as kb:
        kb.write(MODULE_HEADER)

        kb.write('\n')

        render_meals(kb, recipes)
        kb.write('\n')
        render_main_ingredients(kb, recipes)
        kb.write('\n')
        render_additional_ingredients(kb, recipes)


def render_meals(kb, recipes):
    pred = PREDICATE_MEAL

    for recipe_name, recipe in recipes.items():
        name = recipe_name
        meal = recipe['meal']
        kb.write(f'{pred}(\'{name}\',\'{meal}\').\n')


def render_main_ingredients(kb, recipes):
    pred = PREDICATE_MAIN_INGREDIENTS

    for recipe_name, recipe in recipes.items():
        ingredients = []
        for ingredient_name, ingredient in recipe['ingredients']['main'].items():
            name = ingredient_name
            unit = ingredient['unit']
            quantity = ingredient['quantity']
            ingredients.append(f'[\'{name}\',\'{unit}\',{quantity}]')

        ingredients = f',\n{space(4)}'.join(ingredients)
        ingredients = f'{space(4)}{ingredients}'

        kb.write(f'{pred}(\'{recipe_name}\',[\n{ingredients}\n]).\n')


def render_additional_ingredients(kb, recipes):
    pred = PREDICATE_ADDITIONAL_INGREDIENTS

    for recipe_name, recipe in recipes.items():
        if 'additional' in recipe['ingredients']:
            outer_ingredients = []
            for ingredients_group in recipe['ingredients']['additional']:
                inner_ingredients = []
                for ingredient_name, ingredient in ingredients_group.items():
                    name = ingredient_name
                    unit = ingredient['unit']
                    quantity = ingredient['quantity']
                    inner_ingredients.append(f'[\'{name}\',\'{unit}\',{quantity}]')

                inner_ingredients = f',\n{space(8)}'.join(inner_ingredients)
                inner_ingredients = f'{space(8)}{inner_ingredients}'
                
                outer_ingredients.append(inner_ingredients)


            outer_ingredients = f'\n{space(4)}],\n{space(4)}[\n'.join(outer_ingredients)
            outer_ingredients = f'{space(4)}[\n{outer_ingredients}\n{space(4)}]'

            kb.write(f'{pred}(\'{recipe_name}\',[\n{outer_ingredients}\n]).\n')
        else:
            kb.write(f'{pred}(\'{recipe_name}\',[]).\n')


def space(n):
    return ' ' * n
