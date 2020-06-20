.PHONY: deps

PWD = $(shell pwd)
DATA_DIR = data 

DB = redis:6.0
DB_NAME = redis
DB_LOCAL_PORT = 6379
DB_SOURCE_PORT = 6379

LANGS = ru,en

validate_data: deps
	@ mix run --no-compile --no-start scripts/validate_data.exs $(PWD)/$(DATA_DIR) $(LANGS)

populate_data: deps validate_data
	@ mix run --no-compile --no-start scripts/populate_data.exs $(PWD)/$(DATA_DIR) $(LANGS)

db:
	@ docker run -d --name $(DB_NAME) -p $(DB_LOCAL_PORT):$(DB_SOURCE_PORT) $(DB)

deps:
	@ mix deps.get
	@ mix deps.compile
