import argparse
import subprocess
import json

from tinydb import TinyDB, Query
from bottle import get, request, response, run

CALS_LEFT_BIAS = 0.90
CALS_RIGHT_BIAS = 1.10

PROTS_LEFT_BIAS = 0.90
PROTS_RIGHT_BIAS = 1.10

FATS_LEFT_BIAS = 0.90
FATS_RIGHT_BIAS = 1.10

CARBS_LEFT_BIAS = 0.90
CARBS_RIGHT_BIAS = 1.10

parser = argparse.ArgumentParser()
parser.add_argument('--port', type=int, required=True)
parser.add_argument('--db', required=True)
args = parser.parse_args()

db = TinyDB(args.db)

@get('/menu/generate')
def generate_menu():
    cals = float(request.query.cals)
    prots = float(request.query.prots)
    fats = float(request.query.fats)
    carbs = float(request.query.carbs)

    q = Query()
    result = db.search(
        (q.cals > cals * CALS_LEFT_BIAS) &
        (q.cals < cals * CALS_RIGHT_BIAS) &

        (q.prots > prots * PROTS_LEFT_BIAS) &
        (q.prots < prots * PROTS_RIGHT_BIAS) &

        (q.fats > fats * FATS_LEFT_BIAS) &
        (q.fats < fats * FATS_RIGHT_BIAS) &

        (q.carbs > carbs * CARBS_LEFT_BIAS) &
        (q.carbs < carbs * CARBS_RIGHT_BIAS)
    )

    result = list(map(lambda x: x['meals'], result))

    response.content_type = 'application/json'
    response.status = 200

    return json.dumps(result)


def exec(nutritions, excluded_recipes):
    (cals, prots, fats, carbs) = nutritions
    cmd = f'swipl {prog} -- {cals} {prots} {fats} {carbs} {excluded_recipes}'
    resp = subprocess.run(cmd, stdout=subprocess.PIPE, shell=True)

    return (resp.returncode, resp.stdout)

run(host='0.0.0.0', port=args.port, debug=True)
