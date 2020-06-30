.PHONY: deps

PWD = $(shell pwd)
DATA_DIR = data
KNOWLEDGE_BASE_DIR = knowledge_base

DB = redis:6.0
DB_NAME = redis
DB_LOCAL_PORT = 6379
DB_SOURCE_PORT = 6379

LANGS = ru,en

make global_replace:
	@ find $(TARGET) -type f -name "*.$(FILETYPE)" -exec sed -i '' -e 's/$(FROM)/$(TO)/g' {} +

validate_data: deps
	@ mix run --no-compile --no-start scripts/validate_data.exs $(PWD)/$(DATA_DIR) $(LANGS)

populate_data: deps validate_data
	@ mix run --no-compile --no-start scripts/populate_data.exs $(PWD)/$(DATA_DIR) $(LANGS)

render_knowledge_base: deps validate_data
	@ mix run --no-compile --no-start scripts/render_knowledge_base.exs \
		$(PWD)/$(DATA_DIR) $(PWD)/$(KNOWLEDGE_BASE_DIR)

generate_menu:
	@ swipl -s $(KNOWLEDGE_BASE_DIR)/menu.pl -- 1 1 1 1

db:
	@ docker run -d --name $(DB_NAME) -p $(DB_LOCAL_PORT):$(DB_SOURCE_PORT) $(DB)

deps:
	@ mix deps.get
	@ mix deps.compile
