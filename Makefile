PWD = $(shell pwd)
DATA_DIR = data 

check_data_consistency: compile
	@ mix run --no-start scripts/check_data_consistency.exs $(PWD)/$(DATA_DIR)

compile:
	mix compile
