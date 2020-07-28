import argparse
import hashlib
import json

from tinydb import TinyDB


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--data-file')
    parser.add_argument('--db-file')
    args = parser.parse_args()

    db = TinyDB(args.db_file)

    with open(args.data_file, 'r') as data:
        records = list(map(format_record, json.load(data)))
        db.insert_multiple(records)


def format_record(record):
    checksum = hashlib.md5(json.dumps(record).encode()).hexdigest()

    return {
        'checksum': checksum,
        'cals': record['nutritions']['calories'],
        'prots': record['nutritions']['proteins'],
        'fats': record['nutritions']['fats'],
        'carbs': record['nutritions']['carbohydrates'],
        'meals': record['meals']
    }


if __name__ == '__main__':
    main()
