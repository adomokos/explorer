mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

db.rebuild: ## Rebuilds the DBs
	@echo 'Rebuilding the DB...'
	@sh resources/rebuild-db.sh run
	@echo 'Done.'
.PHONY: rebuild-dbs

db.console: ## Open the db-console
	sqlite3 db/explorer-db.sqlt
.PHONY: db-console

build: ## Build with Stack
	stack build --fast
.PHONY: build

test: ## Run the tests
	stack build --fast --test --test-arguments=--format=progress -j4
	# stack build --fast --test
.PHONY: test

repl: ## Run a REPL for development
	stack ghci :$(current_dir)-exe $(current_dir):lib
.PHONY: repl

repl-test: ## Run a REPL with tests
	stack ghci $(current_dir):lib :$(current_dir)-test
.PHONY: repl-test

run: build ## Run app locally
	# stack exec -- $(current_dir)-exe --help # optionally user flags as command line arguments
	stack exec -- $(current_dir)-exe -v
.PHONY: run

create-tags: ## Creates tags for ctags = better navigation in Neovim
	hasktags --ctags .
.PHONY: create-tags

help: ## Prints this help message
	@grep -h -E '^[a-zA-Z0-9\._-]+:.*?## .*$$' $(MAKEFILE_LIST) |\
		sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help
