# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

.PHONY: clean deps help

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT := rocksdb

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)]).")

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC = clang
	CXX = clang++
	CFLAGS ?= -O3 -std=c99 -Wall -Wmissing-prototypes 
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -Wall
	CXXFLAGS += -std=c++11 -stdlib=libc++ -D_GCLIBCXX_USE_C99
else ifeq ($(UNAME_SYS), OpenBsd)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes 
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
else ifeq ($(UNAME_SYS), DragonFly)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes 
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
else ifeq ($(UNAME_SYS), NetBSD)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes 
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99 
else ifeq ($(UNAME_SYS), SunOS)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes 
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

ROCKSDBLIBS = rocksdb/librocksdb.a system/lib/libsnappy.a

ROCKSDB_PLATFORM_LDFLAGS=$(shell cat rocksdb/make_config.mk | grep PLATFORM_LDFLAGS | cut -d' ' -f2- | sed -e 's/-lsnappy//')

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I rocksdb/include
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I rocksdb/include -std=c++11

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei -lstdc++ $(ROCKSDBLIBS)
LDFLAGS += -shared

LDFLAGS += $(ROCKSDB_PLATFORM_LDFLAGS)

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -maxdepth 1 -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

help: ## This documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: $(C_SRC_OUTPUT) ## Build all

$(C_SRC_OUTPUT): $(ROCKSDBLIBS) $(OBJECTS) 
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDLIBS)  $(LDFLAGS) -o $(C_SRC_OUTPUT)

%.o: %.c rocksdb
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

$(ROCKSDBLIBS): deps

deps: ## Build deps for rocksdb
ifdef BIN_ROCKSDB
	if [ ! -d rocksdb ]; then \
		tar -xzf $(BIN_ROCKSDB);\
	fi
else
	./build_deps.sh
endif

clean: ## Clean all files
	./build_deps.sh clean
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)

tar: all ## Creates a tar.gz file containing a pre-build rocksdb library (with src)
	set -e ;\
	TAG=$$(git describe --tags) ;\
	TAR_FILE=rocksdb-e-$$TAG.tar.gz ;\
	tar -cvzf $$TAR_FILE rocksdb/* system/*;\
	echo "";\
	echo "Created tar archive $$TAR_FILE";\
	echo 'Reuse with BIN_ROCKSDB variable:';\
	echo '$ BIN_ROCKSDB=<path to tar> rebar3 compile';\
	echo '$ BIN_ROCKSDB=http://<url to tar> rebar3 compile';\
