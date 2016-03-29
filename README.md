
<img src="https://raw.githubusercontent.com/barrel-db/media/master/banner/barrel-banner-groupfb.png">

<p align="center">
   <a href="https://travis-ci.org/barrel-db/barrel-platform">
        <img src="https://img.shields.io/travis/barrel-db/barrel-platform.svg?style=flat-square">
   </a>
   
   <a href="https://github.com/barrel-db/barrel-platform/blob/master/LICENSE">
        <img src="https://img.shields.io/badge/license-Apache--2.0-blue.svg?style=flat-square">
   </a>

   <a href="https://twitter.com/barreldb">
        <img src="https://img.shields.io/badge/twitter-%40barreldb-55acee.svg?style=flat-square">
   </a>

   <a href="https://docs.barrel-db.org/">
        <img src="https://img.shields.io/badge/docs-readme.io-ff69b4.svg?style=flat-square">
   </a>
</p>

<p align="center"><i>A document-oriented database targeting data locality & P2P</i> - <a href="https://barrel-db.org/" target="_blank">barrel-db.org</a></p>


## Features

- The familiarity of CouchDB
- Bend to your own needs
- Get changes and replicate from a view index
- Optimised for mobile and tablet usage
- Liberal Apache-2.0 license


## Goals

Barrel should be able to work in unreliable conditions when the
quorum can't be achieved (such as offline, disconnected or in other
adverse conditions). The goal is to support efficiency for small and 
large data systems through:

- Maintaining the CouchDB HTTP API
- Modern features such as the view changes based replication
- Erlang OTP support
- P2P distribution


## Requirements

- OS supported: Linux, OSX, BSDs
- Erlang 18.1
- cURL
- ICU
- Spidermonkey 1.8.5


## Installation

 - [Install Erlang](https://docs.barrel-db.org/docs/install-erlang)
 - [Installing Barrel (Source)](https://docs.barrel-db.org/docs/installing-barrel-from-sources)


## Get Involved!

* Star this project :star:
* Try out barrel and [report issues](https://barrel-db.atlassian.net)
* Join the [Barrel User Forum](https://users.barrel-db.org/)
* Request a feature and share ideas
* Pick an [issue on JIRA](https://barrel-db.atlassian.net/) and get hacking
* Follow us on [twitter](https://twitter.com/barreldb)


## Ownership and License

We use the [C4.1 (Collective Code Construction
Contract)](http://rfc.zeromq.org/spec:22) process for contributions. Please read and get involved.

Barrel is distributed under the Apache-2.0 license, see [LICENSE](https://github.com/pjhampton/barrel-platform/blob/couch-1.x/LICENSE).