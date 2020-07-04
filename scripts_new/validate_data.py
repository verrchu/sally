import argparse

from loaders import data_loader


def main():
    (data_dir, langs) = args()

    codes = data_loader.load_codes(data_dir, langs)
    steps = data_loader.load_steps(data_dir, langs)
    measures = data_loader.load_measures(data_dir, langs)
    ingredients = data_loader.load_ingredients(data_dir)
    recipes = data_loader.load_recipes(data_dir)

    print(recipes)


def args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--data-dir')
    parser.add_argument('--langs')
    args = parser.parse_args()

    langs = args.langs.split(',')

    return (args.data_dir, langs)


if __name__ == '__main__':
    main()
