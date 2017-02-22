
<img src="https://raw.githubusercontent.com/barrel-db/media/master/banner/barrel-banner-groupfb.png">

<p align="center">

   <a href="https://gitter.im/barrel-db/barrel-platform?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge">
        <img src="https://badges.gitter.im/barrel-db/barrel-platform.svg">
    </a>
   
   <a href="https://travis-ci.org/barrel-db/barrel-platform">
        <img src="https://travis-ci.org/barrel-db/barrel-platform.svg?branch=master">
   </a>
   
   <a href="https://waffle.io/barrel-db/barrel-platform">
    <img src="https://badge.waffle.io/barrel-db/barrel-platform.png?label=Merging...&title=Merging">
   </a>
      
   <a href="https://gitlab.com/barrel-db/barrel/blob/master/LICENSE">
        <img src="https://img.shields.io/badge/license-Apache--2.0-blue.svg">
   </a>

   <a href="https://twitter.com/barreldb">
        <img src="https://img.shields.io/badge/twitter-%40barreldb-55acee.svg">
   </a>
</p>

<p align="center"><i>A document-oriented database targeting data locality & P2P</i> - <a href="https://barrel-db.org/" target="_blank">barrel-db.org</a></p>

# Barrel platform

Barrel is a modern document-oriented database in Erlang focusing on data locality (put/match the data next to you) and P2P.

Barrel must also be able to work in unreliable conditions were sometimes the quorum can't be achieved (because it is working offline or in other conditions).

Because Barrel is built on an existing relatively small code base, it is possible to make radical changes as part of an incremental process. Underpinning, all this work is efficient for small and large data systems — something rare among database systems.

## Requirements

- OS supported: Linux, OSX, BSDs
- Erlang 19.2

## Prerequisites

This repository contains an [Erlang](https://www.erlang.org/) project packaged
with [rebar3](https://www.rebar3.org/). You need to have Erlang 19.1  and the latest version of
[rebar3](http://rebar3.org) installed to be able to create a release.

## Quickstart

    $ make rel
    $ ./_build/default/rel/barrel/bin/barrel start

## Building a release

Execute the following command line:

    $ make rel

The generated release can be found in the folder `_build/prod/rel` .

> to build a development release, run the command line `make devrel` .
> Please note that this release can't be shipped outside of the current project,
> the release will be found in the folder `_build/default/rel`.

## Testing a release

To start a barrel http server:

    $ ./_build/prod/rel/barrel/bin/barrel start

To stop it:

    $ ./_build/prod/rel/barrel/bin/barrel stop

List of available commands:

    $ ./_build/prod/rel/barrel/bin/barrel

You can consult the embedded [Swagger](http://swagger.io/) page at
http://localhost:7080/api-docs

## Packaging an autonomous tar file

This command create a tarbal including barrek, erlang and associated libs:

    $ make tar

You can deploy the tarball wherever you want:

    $ mkdir barrelprod
    $ cd barrelprod
    $ tar -xzf ../barrel-0.1.0.tar.gz
    $ bin/barrel_http start

