.PHONY: deps

PWD = $(shell pwd)
SCRIPTS_DIR = scripts
DATA_DIR = data
KNOWLEDGE_BASE_DIR = knowledge_base

validate_data:
	@ python $(SCRIPTS_DIR)/validate_data.py \
		--data-dir $(PWD)/$(DATA_DIR)

render_knowledge_base: validate_data
	@ python $(SCRIPTS_DIR)/render_knowledge_base.py \
		--data-dir $(PWD)/$(DATA_DIR) \
		--knowledge-base-dir $(PWD)/$(KNOWLEDGE_BASE_DIR)

generate_menu:
	@ swipl -s $(KNOWLEDGE_BASE_DIR)/menu.pl -- \
		$(CALORIES) $(PROTEINS) $(FATS) $(CARBOHYDRATES) \
		$(EXCLUDED_RECIPES)

server:
	@ python server.py \
		--port $(PORT) --program $(PWD)/$(KNOWLEDGE_BASE_DIR)/menu.pl
		

build:
	docker build --no-cache -t ulidity/sally:latest --build-arg SSH_KEY="$(SSH_KEY)" .
