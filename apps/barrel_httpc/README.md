# barrel_httpc

Barrel HTTP client for Erlang applications

## Build

    $ rebar3 compile

## Post an issue

Issues should be posted on barrel-platform dashboard with the tag `Erlang HTTP client`


## Split to standalone repo

The official repo to update this library is
the [barrel-platform](https://gitlab.com/barrel-db/barrel-platform)
mono-repository.
A [standalone repository](https://gitlab.com/barrel-db/barrel_httpc) is also
available for erlang and elixir applications which want to access a barrel node
without getting the whole platform dependency.

This section describes the procedure to split the libraty for the original
repository and push it to the standalone. It has been inspired
by [this presentation](https://speakerdeck.com/fabpot/a-monorepo-vs-manyrepos).

### Setup

First install [splitsh](https://github.com/splitsh/lite) following the
information provided on its home page.

Then create a remote for barrel_httpc in your barrel-platform repo:

    $ cd barrel-platform
    $ git remote add barrel_httpc git@gitlab.com:barrel-db/barrel_httpc.git

### Split and push

Each time you want to update the barrel_httpc standalone repo, perform the
following commands:

    $ cd barrel-platform
    $ splitsh-lite --prefix apps/barrel_httpc
    $ git subtree push --prefix apps/barrel_httpc barrel_httpc master
