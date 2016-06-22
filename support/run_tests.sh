#!/bin/sh
if [[ $BUILD == 'elixir' ]]; then
  mix test
else
  - ./rebar3 eunit --dir="test"
  - ./rebar3 ct
fi