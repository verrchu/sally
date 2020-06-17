PWD = $(shell pwd)
DATA_DIR = data 

validate_data: deps
	@ mix run --no-compile --no-start scripts/validate_data.exs $(PWD)/$(DATA_DIR)

deps:
	@ mix deps.get
	@ mix deps.compile
