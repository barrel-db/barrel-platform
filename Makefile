BASEDIR = $(shell pwd)
SUPPORTDIR = $(BASEDIR)/support
REBAR ?= $(SUPPORTDIR)/rebar3

OTP_VERSION?=19.3
BUILD_NAME?=working
KERL_DEFAULT_INSTALL_DIR?=$(HOME)/.kerl/local/$(OTP_VERSION)/otp
KERL_CONFIGURE_OPTIONS?=" --disable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --with-wx --without-odbc --enable-dirty-schedulers"

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

morning: distclean clean
	. $(KERL_DEFAULT_INSTALL_DIR)/activate
	$(MAKE) $(MAKE_FLAGS) shell

erlclean:
	kerl delete build $(BUILD_NAME)


install_erlang:
	KERL_CONFIGURE_OPTIONS=$(KERL_CONFIGURE_OPTIONS) \
	KERL_DEFAULT_INSTALL_DIR=$(KERL_DEFAULT_INSTALL_DIR) \
	kerl build $(OTP_VERSION) $(BUILD_NAME)
	kerl install $(BUILD_NAME) $(KERL_DEFAULT_INSTALL_DIR)
	. $(KERL_DEFAULT_INSTALL_DIR)/activate

shell:
	@$(REBAR) as dev shell --sname barrel


dialyzer:
	@$(REBAR) dialyzer

test: cleantest dialyzer eunit ct

eunit:
	@$(REBAR) as dev eunit

ct:
	@$(REBAR) as dev ct

suite:
	. $(KERL_DEFAULT_INSTALL_DIR)/activate
	@$(REBAR) as dev ct --suite $(PWD)/apps/barrel/test/replicate/barrel_replicate_SUITE.erl
cover:
	@$(REBAR) cover


help: ## This documentation
	@echo Build commands for barrel platform:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
	@echo
	@echo Default command is \'compile\'
	@echo Consult README.md for more information.
