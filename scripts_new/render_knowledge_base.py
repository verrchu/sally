import argparse


import lib.loaders.data_loader as data_loader

import lib.knowledge_base.ingredients as ingredients_kb
import lib.knowledge_base.recipes as recipes_kb

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--data-dir')
    parser.add_argument('--knowledge-base-dir')
    args = parser.parse_args()

    ingredients = data_loader.load_ingredients(args.data_dir)
    recipes = data_loader.load_recipes(args.data_dir)

    ingredients_kb.render(args.knowledge_base_dir, ingredients['regular'])
    recipes_kb.render(args.knowledge_base_dir, recipes)


if __name__ == '__main__':
    main()
