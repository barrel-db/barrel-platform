BASEDIR = $(shell pwd)
LIBDIR = $(BASEDIR)/_build/default/lib
ERLC ?= $(shell which erlc)
ESCRIPT ?= $(shell which escript)

REBAR ?= ./rebar3


all: compile

compile:
	@$(REBAR) compile

devrel:
	@$(REBAR) release

rel: clean
	@$(REBAR) as prod release

clean: static_clean
	@$(REBAR) clean

static_clean:
	@rm -rf .libs

distclean: clean
	@rm -rf .dists

tar: rel
	@$(REBAR) as prod tar

.PHONY: tar