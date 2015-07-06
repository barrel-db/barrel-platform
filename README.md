# Barrel

## What can Barrel do

Work on a CouchDB flavor with modern features. Barrel started as a fork of
Apache CouchDB 1.3.

## Features

- compatible with the CouchDB HTTP API
- **view changes**: Get changes and replicate from a view index of your database
- optimised for mobile usage
- easily extendable for your own needs
- Fully opensource. All the sources are on barrel GIT repository
  (http://github.com/barrel) under the Apache License, Version 2.0.


## BARREL Project Goals

Barrel is an effort to maintain the CouchDB HTTP API along with modern features
such as the view changes based replication, full Erlang OTP support, a revised
storage, P2P distribution... Underpinning, all this work is efficiency for
small and large data systems -- something rare among database systems.

Barrel must also be able to work in unreliable conditions where sometimes the
quorum can't be achieved (because it is working offline or in other
condtions).

Because Barrel is built on an existing relatively stable code base, it is
possible to make these radical changes as part of an incremental process.




The main BARREL website is http://barrel-db.org

## Requirements

- OS supported: Linux, OSX, BSDs (windows support is coming)
- Erlang R15
- Curl
- ICU
- Spidermonkey 1.8.5


## Installation

Installation is pretty simple. Just run the command line:

    $ make rel

and it will generate an barrel release in `_build/prod/rel/barrel`. 

To build a compressed tar archive of a release build of project, run the
following command line:

    $ make tar

## Binding port 80

On most UNIX systems binding port 80 is a privileged operation (requires
root). Running Erlang as root is not recommended so some configuration
will need to be done so that rcouch can bind port 80.

If you run a recent Linux kernel with capabilities you can give Erlang
the necessary privilege using the setcap command (you may need to install a
package named lxc, libcap2-bin or similar to obtain this command).

    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam`
    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam.smp

On FreeBSD all ports can be made accessible to all users by issuing:

    $ sysctl net.inet.ip.portrange.reservedhigh=0


## Ownership and License

The contributors are listed in AUTHORS. This project uses the Apache License 2
license, see LICENSE.

Barrel uses the [C4.1 (Collective Code Construction
Contract)](http://rfc.zeromq.org/spec:22) process for contributions.

## Development

Under C4.1 process, you are more than welcome to help us by:

* join the discussion over anything from design to code style try out
* and [submit issue reports](https://github.com/barrel/barrel/issues/new)
* or feature requests pick a task in
* [issues](https://github.com/barrel/barrel/issues) and get it done fork
* the repository and have your own fixes send us pull requests and even
* star this project ^_^

To  run the test suite:

```
    $ make test
```
