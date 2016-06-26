#!/bin/sh

curl -O -L https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
./rebar3 update
./rebar3 release
/home/travis/build/barrel-db/barrel-platform/_build/default/rel/barrel/bin/barrel start
