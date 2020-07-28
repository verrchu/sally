import argparse
import hashlib
import json

from tinydb import TinyDB
from progress.bar import Bar as ProgressBar


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--data-file')
    parser.add_argument('--db-file')
    args = parser.parse_args()

    db = TinyDB(args.db_file)

    with open(args.data_file, 'r') as data:
        data = json.load(data)
        progress_bar = ProgressBar('RECORDS', max=len(data))
        for record in data:
            checksum = hashlib.md5(json.dumps(record).encode()).hexdigest()
            db.insert({
                'checksum': checksum,
                'cals': record['nutritions']['calories'],
                'prots': record['nutritions']['proteins'],
                'fats': record['nutritions']['fats'],
                'carbs': record['nutritions']['carbohydrates'],
                'meals': record['meals']
            })
            progress_bar.next()
        progress_bar.finish()


if __name__ == '__main__':
    main()
