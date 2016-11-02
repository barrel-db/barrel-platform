PROJECT = example

DEPS = cowboy_swagger mixer

dep_mixer          = git https://github.com/inaka/mixer.git 0.1.4
dep_cowboy_swagger = git https://github.com/inaka/cowboy-swagger.git

include erlang.mk

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config config/sys.config
