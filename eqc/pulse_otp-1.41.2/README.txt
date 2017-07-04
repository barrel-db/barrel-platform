The files in this application are modified versions of Erlang OTP
components, originally provided by Ericsson AB, suitable for
instrumenting with the PULSE tool. Each file pulse_XXX.erl is a
modified version of XXX.erl from the Erlang OTP distribution. The
modules are taken from Erlang R15B, with some call-backs commented out
to permit compilation under R14B also. The modifications are copyright
Quviq AB.

The changes to the original modules include:
 -  renaming atoms (xxxx to pulse_xxxx)
 -  renaminge ETS table names
 -  changes in pulse_application to look for applications in both the 
    pulse_application table and the normal application table. This is 
    needed because applications often depend other applications, and
    we do not with to start, for example, an instrumented version of
    stdlib just in order to start an instrumented application that
    depends on it. 
 -  Adding a compilation time to ensure the instrumented code was
    instrumented by the running version of PULSE.
