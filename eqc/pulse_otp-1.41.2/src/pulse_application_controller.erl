-file("pulse_otp-1.41.2/src/pulse_application_controller.erl", 0).
-module(pulse_application_controller).

-export([compiled/0]).
-export([start/0]).
-compile({parse_transform, pulse_instrument}).
-compile({pulse_replace_module, [{real_application, application}]}).
-include("../include/pulse_otp.hrl").

-define(ac_tab, pulse_ac_tab).

-ifdef(OTP_R17).
-include("pulse_application_controller_r17.erl").

-else.
-ifdef(OTP_R18).
-include("pulse_application_controller_r18.erl").

-else.
-ifdef(OTP_R19).
-include("pulse_application_controller_r19.erl").

-else.
-ifdef(OTP_R20).
-include("pulse_application_controller_r19.erl").

-else.
-error("Unsupported OTP release").
-endif.
-endif.
-endif.
-endif.

%% Called by pulse_application:load/2.
start() ->
  start({application, kernel, []}).

compiled() -> ?COMPILED.


