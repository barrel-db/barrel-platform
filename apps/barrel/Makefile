BASEDIR = $(shell pwd)
SUPPORTDIR = $(BASEDIR)/support
REBAR ?= $(SUPPORTDIR)/rebar3

.PHONY: help all rel tar store apply

all: compile

compile:
	@$(REBAR) compile

## Create a barrel release
rel:
	@$(REBAR) as prod release

devrel: ## Create a barrel release
	@$(REBAR) release

tar: ## Create a tar file containing a portable release
	@$(REBAR) as prod tar

clean:
	@$(REBAR) clean

distclean: clean ## Clean all build and releases artifacts
	rm -rf _build

cleantest:
	@rm -rf _build/test

dialyzer:
	@$(REBAR) dialyzer

test: cleantest dialyzer
	@$(REBAR) eunit
	@$(REBAR) ct

cover:
	@$(REBAR) cover


help: ## This documentation
	@echo Build commands for barrel platform:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
	@echo
	@echo Default command is \'compile\'
	@echo Consult README.md for more information.
