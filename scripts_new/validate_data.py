import argparse

from lib.loaders import data_loader, schema_loader
from lib.validators import (
    measure_validator,
    ingredient_validator,
    recipe_validator
)


def main():
    (data_dir, langs) = args()

    codes = data_loader.load_codes(data_dir, langs)
    measures = data_loader.load_measures(data_dir, langs)
    ingredients = data_loader.load_ingredients(data_dir)
    recipes = data_loader.load_recipes(data_dir)
    recipes_steps = data_loader.load_recipes_steps(data_dir, langs)

    schemas = schema_loader.load_schemas(data_dir)

    measure_validator.validate(measures, schemas)
    ingredient_validator.validate(ingredients, codes, schemas)
    recipe_validator.validate(recipes, ingredients, recipes_steps, codes, schemas)


def args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--data-dir')
    parser.add_argument('--langs')
    args = parser.parse_args()

    langs = args.langs.split(',')

    return (args.data_dir, langs)


if __name__ == '__main__':
    main()
