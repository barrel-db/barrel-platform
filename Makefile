BASEDIR = $(shell pwd)
LIBDIR = $(BASEDIR)/_build/default/lib
ERLC ?= $(shell which erlc)
ESCRIPT ?= $(shell which escript)

all: compile

compile:
	./rebar3 compile

update:
	./rebar3 update

devrel:
	./rebar3 release

rel: clean
	./rebar3 as prod release

clean: static_clean
	./rebar3 clean

static_clean:
	@rm -rf .libs

distclean: clean
	@rm -rf .dists

tar: rel
	./rebar3 as prod tar

.PHONY: tar
