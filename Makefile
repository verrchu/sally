.PHONY: deps

PWD = $(shell pwd)
SCRIPTS_DIR = $(PWD)/scripts
DATA_DIR = $(PWD)/data
KNOWLEDGE_BASE_DIR = $(PWD)/knowledge_base
TMP_DIR = $(PWD)/tmp
TMP_DATA_FILE = $(TMP_DIR)/data
TMP_DB_FILE = $(TMP_DIR)/db

validate_data:
	@ python $(SCRIPTS_DIR)/validate_data.py \
		--data-dir $(DATA_DIR)

render_knowledge_base: validate_data
	@ python $(SCRIPTS_DIR)/render_knowledge_base.py \
		--data-dir $(DATA_DIR) \
		--knowledge-base-dir $(KNOWLEDGE_BASE_DIR)

generate_menu:
	@ swipl -t "menu:main" $(KNOWLEDGE_BASE_DIR)/menu.pl | jq --slurp . > $(TMP_DATA_FILE)

prune_db:
	@ rm -f $(TMP_DB_FILE)

populate_db:
	@ python $(SCRIPTS_DIR)/populate_db.py \
		--data-file $(TMP_DATA_FILE) --db-file $(TMP_DB_FILE)

test:
	@ swipl -t "load_test_files([]), run_tests." $(KNOWLEDGE_BASE_DIR)/menu.pl

server:
	@ python server.py --port $(PORT) --db $(TMP_DB_FILE)

build:
	docker build --no-cache -t ulidity/sally:latest .
