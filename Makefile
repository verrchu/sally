PWD = $(shell pwd)
DATA_DIR = data 

check_data_consistency:
	@ mix run --no-compile --no-start scripts/check_data_consistency.exs $(PWD)/$(DATA_DIR)
