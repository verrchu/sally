import jsonschema

def validate_schema(data, schema):
    jsonschema.validate(data, schema)
