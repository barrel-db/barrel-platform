# Barrel platform

This is the main package for [Barrel-DB](https://barrel-db.org/).

## Prerequisites

This repository contains an [Erlang](https://www.erlang.org/) project packaged
with [rebar3](https://www.rebar3.org/). You need to have Erlang installed to be
able to create a release.

## Building a release

   $ ./rebar3 release

## Testing a release

To start a barrel http server:

   $ ./build/default/rel/barrel/bin/barrel_http start

To stop it:

   $ ./build/default/rel/barrel/bin/barrel_http stop

List of available commands:

   $ ./build/default/rel/barrel/bin/barrel_http


You can consult the embedded [Swagger](http://swagger.io/) page at
http://localhost:8080/api-docs

![Swagger](/doc/swagger.png)

## Packaging an autonomous tar file

(to do)

