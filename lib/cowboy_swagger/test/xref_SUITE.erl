-module(xref_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([xref/1]).

-spec all() -> [xref].
all() -> [xref].

-spec xref(lsl_test_utils:config()) -> {comment, []}.
xref(_Config) ->
  Dirs = [filename:absname("../../ebin")],
  [] = xref_runner:check(undefined_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(locals_not_used, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_functions, #{dirs => Dirs}),
  {comment, ""}.
