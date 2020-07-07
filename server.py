import argparse
import subprocess
import json

import base64

from bottle import get, request, response, run


parser = argparse.ArgumentParser()
parser.add_argument('--port', type=int, required=True)
parser.add_argument('--path', required=True)
args = parser.parse_args()


@get('/menu/generate')
def generate_menu():
    excluded_recipes = base64.decodebytes(
        (request.query.er or '').encode()
    ).decode().strip()
    cals = float(request.query.nc)
    prots = float(request.query.np)
    fats = float(request.query.nf)
    carbs = float(request.query.nb)

    menu = exec((cals, prots, fats, carbs), excluded_recipes)

    response.content_type = 'application/json'
    response.status = 200
    return menu


def exec(nutritions, excluded_recipes):
    (cals, prots, fats, carbs) = nutritions
    resp = subprocess.run(['make', 'generate_menu'], env = {
        'CALORIES': str(cals),
        'PROTEINS': str(prots),
        'FATS': str(fats),
        'CARBOHYDRATES': str(carbs),
        'EXCLUDED_RECIPES': excluded_recipes
    }, stdout=subprocess.PIPE)
    return resp.stdout

run(host='0.0.0.0', port=args.port, debug=True)
