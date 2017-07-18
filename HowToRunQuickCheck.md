# How to run QuickCheck

In order to run QuickCheck you will need a license and to install QuickCheck.

## To Install QuickCheck

Download QuickCheck from  http://quviq-licencer.com/downloads/eqc.zip

Unzip the file and CD Into the directory

as Root
```
erl
> eqc:install().
```

## Install your licence

As your user in Erlang
```
> eqc:registration("LICENCE ID").
```

## Running Properties

QuickCheck properties are in application directories `eqc`. There is a
`user_default.erl` file that has a few useful commands in it. To run
the QuickCheck properties from the Erlang shell type the command
`eqc()`.


## Running properties with Rebar3

run `rebar3 eqc` (Not yet working)
