import os
from pathlib import Path

from lib.loaders.util import load_yaml


INGREDIENTS_DIR = 'ingredients'
RECIPES_DIR = 'recipes'

INGREDIENT_TYPES = ['regular', 'technical']


def load_ingredients(data_dir):
    data = {}
    for ingredient_type in INGREDIENT_TYPES:
        ingredients_dir = os.path.join(data_dir, INGREDIENTS_DIR, ingredient_type)
        ingredient_files = os.listdir(ingredients_dir)

        ingredients = {}
        for ingredient_file in ingredient_files:
            ingredient_path = os.path.join(ingredients_dir, ingredient_file)
            ingredient_name = get_ingredient_name(ingredient_file)
            ingredient = load_yaml(ingredient_path)

            ingredients[ingredient_name] = ingredient

        data[ingredient_type] = ingredients

    return data


def load_recipes(data_dir):
    recipes_dir = os.path.join(data_dir, RECIPES_DIR)
    recipe_files = os.listdir(recipes_dir)

    data = {}
    for recipe_file in recipe_files:
        recipe_path = os.path.join(recipes_dir, recipe_file)
        recipe_name = get_recipe_name(recipe_file)
        recipe = load_yaml(recipe_path)

        data[recipe_name] = recipe

    return data


def get_ingredient_name(file):
    return Path(file).stem.upper()

def get_recipe_name(file):
    return Path(file).stem.upper()
