PWD = $(shell pwd)
DATA_DIR = data 

LANGS = ru,en

validate_data: deps
	@ mix run --no-compile --no-start scripts/validate_data.exs $(PWD)/$(DATA_DIR) $(LANGS)

populate_data: deps validate_data
	@ mix run --no-compile --no-start scripts/populate_data.exs $(PWD)/$(DATA_DIR) $(LANGS)

deps:
	@ mix deps.get
	@ mix deps.compile
