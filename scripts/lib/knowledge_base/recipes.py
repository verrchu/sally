import os

PREDICATE_MEAL = 'meal'
PREDICATE_SUFFICIENT = 'sufficient'
PREDICATE_INGREDIENTS = 'ingredients'
PREDICATE_VARIANT = 'variant'
PREDICATE_COMPLEMENTS = 'complements'

MODULE_HEADER = f"""
:- module(recipes_kb, [
    {PREDICATE_MEAL}/2,
    {PREDICATE_SUFFICIENT}/1,
    {PREDICATE_INGREDIENTS}/2,
    {PREDICATE_VARIANT}/3,
    {PREDICATE_COMPLEMENTS}/3
]).

:- dynamic {PREDICATE_MEAL}/2.
:- dynamic {PREDICATE_SUFFICIENT}/1.
:- dynamic {PREDICATE_INGREDIENTS}/2.
:- dynamic {PREDICATE_VARIANT}/3.
:- dynamic {PREDICATE_COMPLEMENTS}/3.
"""

KNOWLEDGE_BASE_FILE = "recipes_kb.pl"


def render(kb_dir, recipes):
    kb_path = os.path.join(kb_dir, KNOWLEDGE_BASE_FILE)

    with open(kb_path, 'w') as kb:
        kb.write(MODULE_HEADER)

        kb.write('\n')

        render_sufficient_parameter(kb, recipes)
        kb.write('\n')
        render_meals(kb, recipes)
        kb.write('\n')
        render_ingredients(kb, recipes)
        kb.write('\n')
        render_variants(kb, recipes)
        kb.write('\n')
        render_complements(kb, recipes)


def render_sufficient_parameter(kb, recipes):
    pred = PREDICATE_SUFFICIENT

    for recipe_name, recipe in recipes.items():
        if recipe['sufficient']:
            name = f'"{recipe_name}"'
            kb.write(f'{pred}({name}).\n')


def render_meals(kb, recipes):
    pred = PREDICATE_MEAL

    for recipe_name, recipe in recipes.items():
        if 'meals' in recipe:
            for meal in recipe['meals']:
                name = f'"{recipe_name}"'
                meal = f'"{meal}"'
                kb.write(f'{pred}({name},{meal}).\n')


def render_ingredients(kb, recipes):
    pred = PREDICATE_INGREDIENTS

    for recipe_name, recipe in recipes.items():
        recipe_name = f'"{recipe_name}"'

        ingredients = []
        for ingredient_name, ingredient in recipe['ingredients']['regular'].items():
            name = f'"{ingredient_name}"'
            unit = f'"{ingredient["unit"]}"'
            quantity = ingredient['quantity']
            ingredients.append(f'[{name},{unit},{quantity}]')

        ingredients = f',\n{space(4)}'.join(ingredients)
        ingredients = f'{space(4)}{ingredients}'

        kb.write(f'{pred}({recipe_name},[\n{ingredients}\n]).\n')


def render_variants(kb, recipes):
    pred = PREDICATE_VARIANT

    for recipe_name, recipe in recipes.items():
        recipe_name = f'"{recipe_name}"'

        if 'variants' in recipe:
            for variant_id, variant in recipe['variants'].items():
                ingredients = []
                for ingredient_name, ingredient in variant['ingredients'].items():
                    name = f'"{ingredient_name}"'
                    unit = f'"{ingredient["unit"]}"'
                    quantity = ingredient['quantity']
                    ingredients.append(f'[{name},{unit},{quantity}]')

                ingredients = f',\n{space(4)}'.join(ingredients)
                ingredients = f'[\n{space(4)}{ingredients}\n]'

                variant_id = f'"{variant_id}"'

                if variant['standalone']:
                    kb.write(f'{pred}({recipe_name},[standalone,{variant_id}],{ingredients}).\n')
                if variant['embeddable']:
                    kb.write(f'{pred}({recipe_name},[embeddable,{variant_id}],{ingredients}).\n')


def render_complements(kb, recipes):
    pred = PREDICATE_COMPLEMENTS

    for recipe_name, recipe in recipes.items():
        recipe_name = f'"{recipe_name}"'

        if 'complements' in recipe:
            for complements_id, complements in recipe['complements'].items():
                complements_txt = []
                for complement_name, complement in complements.items():
                    name = f'"{complement_name}"'

                    if complement['variant']:
                        variant_id = complement['variant']
                        variant_id = f'"{variant_id}"'

                        complements_txt.append(f'[{name},{variant_id}]')
                    else:
                        complements_txt.append(f'[{name},none]')


                complements_txt = f',\n{space(4)}'.join(complements_txt)
                complements_txt = f'[\n{space(4)}{complements_txt}\n]'

                complements_id = f'"{complements_id}"'

                kb.write(f'{pred}({recipe_name},{complements_id},{complements_txt}).\n')


def space(n):
    return ' ' * n
