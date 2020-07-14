import os

PREDICATE_MEAL = 'meal'
PREDICATE_MAIN_INGREDIENTS = 'main_ingredients'
PREDICATE_ADDITIONAL_INGREDIENTS = 'additional_ingredients'

MODULE_HEADER = f"""
:- module(recipes_kb, [
    {PREDICATE_MEAL}/2,
    {PREDICATE_MAIN_INGREDIENTS}/2,
    {PREDICATE_ADDITIONAL_INGREDIENTS}/3
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
        name = f'"{recipe_name}"'
        meal = f'"{recipe["meal"]}"'
        kb.write(f'{pred}({name},{meal}).\n')


def render_main_ingredients(kb, recipes):
    pred = PREDICATE_MAIN_INGREDIENTS

    for recipe_name, recipe in recipes.items():
        recipe_name = f'"{recipe_name}"'

        ingredients = []
        for ingredient_name, ingredient in recipe['ingredients']['main'].items():
            name = f'"{ingredient_name}"'
            unit = f'"{ingredient["unit"]}"'
            quantity = ingredient['quantity']
            ingredients.append(f'[{name},{unit},{quantity}]')

        ingredients = f',\n{space(4)}'.join(ingredients)
        ingredients = f'{space(4)}{ingredients}'


        kb.write(f'{pred}({recipe_name},[\n{ingredients}\n]).\n')


def render_additional_ingredients(kb, recipes):
    pred = PREDICATE_ADDITIONAL_INGREDIENTS

    for recipe_name, recipe in recipes.items():
        recipe_name = f'"{recipe_name}"'

        if 'additional' in recipe['ingredients']:
            outer_ingredients = []
            additional_ingredients = recipe['ingredients']['additional']
            for ingredients_group_id, ingredients_group in additional_ingredients.items():
                ingredients = []
                for ingredient_name, ingredient in ingredients_group.items():
                    name = f'"{ingredient_name}"'
                    unit = f'"{ingredient["unit"]}"'
                    quantity = ingredient['quantity']
                    ingredients.append(f'[{name},{unit},{quantity}]')

                ingredients = f',\n{space(4)}'.join(ingredients)
                ingredients = f'[\n{space(4)}{ingredients}\n]'

                ingredients_group_id = f'"{ingredients_group_id}"'

                kb.write(f'{pred}({recipe_name},{ingredients_group_id},{ingredients}).\n')


def space(n):
    return ' ' * n
