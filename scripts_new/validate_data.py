import argparse

from lib.loaders import data_loader, schema_loader
from lib.validators import (
    measures_validator,
    ingredients_validator
)


def main():
    (data_dir, langs) = args()

    codes = data_loader.load_codes(data_dir, langs)
    steps = data_loader.load_steps(data_dir, langs)
    measures = data_loader.load_measures(data_dir, langs)
    ingredients = data_loader.load_ingredients(data_dir)
    recipes = data_loader.load_recipes(data_dir)

    schemas = schema_loader.load_schemas(data_dir)

    measures_validator.validate(measures, schemas)
    ingredients_validator.validate(ingredients, schemas)


def args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--data-dir')
    parser.add_argument('--langs')
    args = parser.parse_args()

    langs = args.langs.split(',')

    return (args.data_dir, langs)


if __name__ == '__main__':
    main()
