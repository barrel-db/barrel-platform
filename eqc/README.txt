To install QuickCheck
=====================

The Easy Way:
-------------

QuickCheck is designed to be installed in the lib directory of your
Erlang installation. The easiest way to do so is to run the
installation program 

eqc_install:install().

This should be run in the same directory that this README file is in.

NOTE: you must have write access to your Erlang installation, which
under Windows normally means you must run Erlang as the administrator
for this to succeed, and under Linux you may need sudo.

If you are installing QuickCheck for the first time, you must also
register a licence identifier, by running

eqc:registration("xxxxxxxxxxxx").

in the shell (where "xxxxxxxxxxxx" is replaced by your licence
identifier). You MUST be running Erlang under your own username when
you do this part--if you register your licence as the Administrator or
superuser, then you will create a licence that only the
Administrator/superuser can use.

To get a licence identifier:

- Ask your Quviq licence administrator, if your employer has purchased
  a licence. 
- Ask support@quviq.com, if you are purchasing your own licence, or
  are the licence administrator and don't know what to do.

Without a licence identifier:

Use www.quickcheck-ci.com an experimental CI system that has QuickCheck 
installed and lets you run QuickCheck in the cloud.

If you experience problems, please mail support@quviq.com.

The Slightly Harder Way:
------------------------

To install QuickCheck somewhere else, choose a directory Dir and call

eqc_install:install(Dir).

as above.

This moves eqc-1.xx.y, pulse-1.xx.y etc into Dir. To use them, you
must make sure that Dir/eqc-1.xx.y/ebin, Dir/pulse-1.xx.y/ebin etc are
in your Erlang code path whenever you start QuickCheck. For example,
you can put a call to code:add_paths in your .erlang file, so these
paths are added every time the Erlang VM starts.

The Hard Way:
-------------

You can carry out the installation steps yourself.

1) Find the directory where you want to place the QuickCheck
applications (eqc, pulse, possibly more). This is normally your Erlang
lib directory, which you can find using, for example, 

code:where_is_file("erlang.beam").

2) Delete any other versions of the QuickCheck applications, or of
eqcmini, from this directory.

2) Move the QuickCheck applications into this directory.

3) Start the Erlang shell, and ensure that the ebin directories of the
QuickCheck applications are in your code path. For example, you can
call code:add_paths/1.

4) Call eqc:version(), and check that you see the version you
expect. Version numbers are returned as floating point, so 1.20.3 for
example is returned as 1.203.

5) Call eqc_emacs_mode:install() if you are an Emacs user.

6) Call eqc:registration if necessary to install your licence
identifier.

7) Call eqc:start(), and check that QuickCheck starts successfully.
