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

clean:
	@$(REBAR) clean

tar: rel
	@$(REBAR) as prod tar

.PHONY: tar
