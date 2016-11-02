#!/usr/bin/env sh

PROJECT=`pwd`
LIB_DIR=$PROJECT/_build/import/lib
REBAR=$PROJECT/support/rebar3

rm  -rf $PROJECT/lib $LIB_DIR

mkdir -p $LIB_DIR
$REBAR clean
$REBAR as import install_deps
cp -RL $LIB_DIR .

find $PROJECT/lib -name "rebar.config" -exec escript $PROJECT/support/remove_deps.escript {} \;
find $PROJECT/lib | grep .git | xargs rm -rf
find $PROJECT/lib | grep rebar.lock | xargs rm -rf
