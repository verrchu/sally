import argparse
import subprocess
import json

import base64

from bottle import get, request, response, run


parser = argparse.ArgumentParser()
parser.add_argument('--port', type=int, required=True)
parser.add_argument('--program', required=True)
args = parser.parse_args()

prog = args.program

@get('/menu/generate')
def generate_menu():
    excluded_recipes = base64.decodebytes(
        (request.query.er or '').encode()
    ).decode().strip()
    cals = float(request.query.nc)
    prots = float(request.query.np)
    fats = float(request.query.nf)
    carbs = float(request.query.nb)

    (exit_code, output) = exec((cals, prots, fats, carbs), excluded_recipes)

    if exit_code == 0:
        response.content_type = 'application/json'
        response.status = 200
    else:
        response.status = 500

    return output


def exec(nutritions, excluded_recipes):
    (cals, prots, fats, carbs) = nutritions
    cmd = f'swipl {prog} -- {cals} {prots} {fats} {carbs} {excluded_recipes}'
    resp = subprocess.run(cmd, stdout=subprocess.PIPE, shell=True)

    return (resp.returncode, resp.stdout)

run(host='0.0.0.0', port=args.port, debug=True)
