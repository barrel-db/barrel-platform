ERL= erl
BASEDIR=$(CURDIR)
PRIVDIR=$(BASEDIR)/../../priv
STATIC_LIBS=$(BASEDIR)/../../../../.libs

BARRELJS_DIR=.

BARRELJS_SOURCES=$(shell find $(BARRELJS_DIR) -type f \( -name "*.c" \))
BARRELJS_OBJS=$(addsuffix .o, $(basename $(BARRELJS_SOURCES)))
BARRELJS_BIN=$(PRIVDIR)/barreljs

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	JSCFLAGS?= -DXP_UNIX -I/usr/include -I/usr/local/include/js
	JSLIBS?= -L/usr/local/lib -lmozjs185
	STATIC_LDFLAGS = -lstdc++
else ifeq ($(UNAME_SYS), Linux)
	JSCFLAGS?= -DXP_UNIX -I/usr/include/js
	JSLIBS?= -lmozjs185 -lm
	STATIC_LDFLAGS = -ldl -lstdc++ -lpthread -lm
else ifeq ($(PLATFORM), freebsd)
	JSCFLAGS?= -DXP_UNIX -I/usr/local/include/js
	JSLIBS?= -L/usr/local/lib -lmozjs185 -lm
	STATIC_LDFLAGS = -lstdc++ -lpthread
else ifeq ($(PLATFORM), openbsd)
	JSCFLAGS?= -DXP_UNIX -I/usr/local/include/js
	JSLIBS?= -L/usr/local/lib -lmozjs185 -lm
else ifeq ($(PLATFORM), netbsd)
	JSCFLAGS?= -DXP_UNIX -I/usr/local/include/js
	JSLIBS?= -L/usr/local/lib -lmozjs185 -lm
endif

ifeq ($(JS_COMPILE), static)
	JSCFLAGS = -DXP_UNIX -I$(STATIC_LIBS)/js/include -I$(STATIC_LIBS)/nsprpub/include
	JSLIBS = -DXP_UNIX $(STATIC_LIBS)/nsprpub/lib/libnspr4.a  $(STATIC_LIBS)/js/lib/libjs_static.a $(STATIC_LDFLAGS)
endif

JSLIBS += -lcurl

all: $(BARRELJS_BIN)

clean:
	@rm -rf $(BARRELJS_SO) $(BARRELJS_OBJS)

distclean: clean
	@rm -rf $(BARRELJS_ENV)

$(BARRELJS_BIN): $(BARRELJS_OBJS)
	@mkdir -p $(PRIVDIR)
	@cd "$(BASEDIR)" && \
		$(CC) $(BARRELJS_OBJS) $(JSLIBS) -o "$(BARRELJS_BIN)"

%.o: %.c
	@cd "$(BASEDIR)" && \
		$(CC) -c $(JSCFLAGS) $<
