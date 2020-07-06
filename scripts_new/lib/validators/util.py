import jsonschema

def validate_schema(data, schema):
    jsonschema.validate(data, schema)


def validate_code(code, codes):
    for lang, codes in codes.items():
        if not code in codes:
            raise Exception(f"""
            CODE IN NOT DEFINED
            CODE: {code}
            LANG: {lang}
            """)
