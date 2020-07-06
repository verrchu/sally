import argparse

from lib.loaders import data_loader, schema_loader
from lib.validators import ingredient_validator, recipe_validator


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--data-dir')
    args = parser.parse_args()

    ingredients = data_loader.load_ingredients(args.data_dir)
    recipes = data_loader.load_recipes(args.data_dir)

    schemas = schema_loader.load_schemas(args.data_dir)

    ingredient_validator.validate(ingredients, schemas)
    recipe_validator.validate(recipes, ingredients, schemas)


if __name__ == '__main__':
    main()
