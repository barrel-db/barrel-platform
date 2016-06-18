#!/bin/sh
if [[ $BUILD == 'elixir' ]]; then
  mix local.hex --force
  mix deps.get
  mix compile
else
  curl -O -L https://s3.amazonaws.com/rebar3/rebar3
  chmod +x rebar3
  ./rebar3 update
fi
