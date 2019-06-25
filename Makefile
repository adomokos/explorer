mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

build: ## Build with Stack
	stack build
.PHONY: build

test: ## Run the tests
	stack build --fast --test --test-arguments=--format=progress -j4
	# stack build --fast --test
.PHONY: test

repl: ## Run a REPL for development
	stack ghci :$(current_dir)-exe
.PHONY: repl

repl-test: ## Run a REPL with tests
	stack ghci :$(current_dir)-test
.PHONY: repl-test

run: ## Run app locally
	stack exec -- $(current_dir)-exe
.PHONY: run

help: ## Display this message
	@echo "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
.PHONY: help
.DEFAULT_GOAL := help
