import os
import yaml
from pathlib import Path


LOCALIZATION_DIR = 'localization'
INGREDIENTS_DIR = 'ingredients'
RECIPES_DIR = 'recipes'

CODES_FILE = 'codes.yaml'
STEPS_FILE = 'steps.yaml'
MEASURES_FILE = 'measures.yaml'

INGREDIENT_TYPES = ['regular', 'technical']


def load_codes(data_dir, langs):
    localization_path = os.path.join(data_dir, LOCALIZATION_DIR)

    data = {}
    for lang in langs:
        codes_path = os.path.join(localization_path, lang, CODES_FILE)
        data[lang] = load_yaml(codes_path)

    return data


def load_steps(data_dir, langs):
    localization_path = os.path.join(data_dir, LOCALIZATION_DIR)

    data = {}
    for lang in langs:
        steps_path = os.path.join(localization_path, lang, STEPS_FILE)
        data[lang] = load_yaml(steps_path)

    return data


def load_measures(data_dir, langs):
    localization_path = os.path.join(data_dir, LOCALIZATION_DIR)

    data = {}
    for lang in langs:
        measures_path = os.path.join(localization_path, lang, MEASURES_FILE)
        data[lang] = load_yaml(measures_path)

    return data


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


def load_yaml(path):
    return yaml.load(open(path, 'r'), Loader=yaml.Loader)

def get_ingredient_name(file):
    return Path(file).stem.upper()

def get_recipe_name(file):
    return Path(file).stem.upper()
