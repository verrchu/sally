import os

PREDICATE_NUTRITION = 'nutrition'

MODULE_HEADER = f"""
:- module(ingredients_kb, [
    {PREDICATE_NUTRITION}/5
]).

:- dynamic {PREDICATE_NUTRITION}/5
"""

KNOWLEDGE_BASE_FILE = "ingredients_kb.pl"


def render(kb_dir, ingredients):
    kb_path = os.path.join(kb_dir, KNOWLEDGE_BASE_FILE)

    with open(kb_path, 'w') as kb:
        kb.write(MODULE_HEADER)

        kb.write('\n')

        nutritions = []
        for ingredient_name, ingredient in ingredients.items():
            name = f'"{ingredient_name}"'
            for unit, unit_data in ingredient['units'].items():
                unit = f'"{unit}"'
                quantity = unit_data['quantity']
                for key, value in unit_data['nutritions'].items():
                    nutritions.append((name, unit, quantity, key, value))

        pred = PREDICATE_NUTRITION
        for name, unit, quantity, key, value in nutritions:
            kb.write(f'{pred}({name},{unit},{quantity},{key},{value}).\n')
