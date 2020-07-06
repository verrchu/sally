.PHONY: deps

PWD = $(shell pwd)
SCRIPTS_DIR = scripts
DATA_DIR = data
KNOWLEDGE_BASE_DIR = knowledge_base

DB = redis:6.0
DB_NAME = redis
DB_LOCAL_PORT = 6379
DB_SOURCE_PORT = 6379

validate_data:
	@ python $(SCRIPTS_DIR)/validate_data.py \
		--data-dir $(PWD)/$(DATA_DIR)

render_knowledge_base: validate_data
	@ python $(SCRIPTS_DIR)/render_knowledge_base.py \
		--data-dir $(PWD)/$(DATA_DIR) \
		--knowledge-base-dir $(PWD)/$(KNOWLEDGE_BASE_DIR)

generate_menu:
	@ swipl -s $(KNOWLEDGE_BASE_DIR)/menu.pl -- \
		$(CALORIES) $(PROTEINS) $(FATS) $(CARBOHYDRATES)

db:
	@ docker run -d --name $(DB_NAME) -p $(DB_LOCAL_PORT):$(DB_SOURCE_PORT) $(DB)
