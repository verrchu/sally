from lib.validators.util import validate_schema


def validate(measures, schemas):
    measure_schema = schemas['measure']

    for lang, measures in measures.items():
        for measure_name, measure in measures.items():
            validate_schema(measure, measure_schema)
