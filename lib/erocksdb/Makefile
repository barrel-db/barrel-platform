REBAR ?= ./rebar

all: compile test

get-deps:
	./c_src/build_deps.sh get-deps

rm-deps:
	./c_src/build_deps.sh rm-deps

deps:
	${REBAR} get-deps

compile:
	${REBAR} compile

test: compile
	${REBAR} eunit

clean:
	${REBAR} clean
