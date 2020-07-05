import os
import jsonref

SCHEMAS_DIR = 'schemas'

SCHEMAS = [
    'recipe',
    'regular_ingredient',
    'technical_ingredient'
]


def load_schemas(data_dir):
    schemas_dir = os.path.join(data_dir, SCHEMAS_DIR)

    data = {}
    for schema_name in SCHEMAS:
        data[schema_name] = load_schema(schemas_dir, schema_name)

    return data


def load_schema(schemas_dir, schema_name):
    schema_file = '{}.json'.format(schema_name)
    schema_path = os.path.join(schemas_dir, schema_file)
    base_uri = 'file://{}/'.format(schemas_dir)
    return jsonref.load(open(schema_path, 'r'), base_uri=base_uri, jsonschema=True)
