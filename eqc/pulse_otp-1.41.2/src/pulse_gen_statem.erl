-file("pulse_otp-1.41.2/src/pulse_gen_statem.erl", 0).
-module(pulse_gen_statem).

-export([compiled/0]).
-compile({parse_transform, pulse_instrument}).
-include("../include/pulse_otp.hrl").

-ifdef(OTP_R19).
-include("pulse_gen_statem_r19.erl").

-else.
-ifdef(OTP_R20).
-include("pulse_gen_statem_r19.erl").

-else.
-error("Unsupported OTP release").
-endif.
-endif.

compiled() -> ?COMPILED.


