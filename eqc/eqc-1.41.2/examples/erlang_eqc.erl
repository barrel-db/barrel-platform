-file("eqc-1.41.2/examples/erlang_eqc.erl", 0).
-module(erlang_eqc).
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(TEST_MODULE, myprog).

compile(Code) ->
  compile(Code, []).

compile(Code, Options) ->
  File = lists:concat([?TEST_MODULE, ".erl"]),
  file:write_file(File, Code),
  compile:file(File, Options).

prop_compile() ->
  ?FORALL(Code, eqc_erlang_program:module(?TEST_MODULE, [macros, maps, recursive_funs]),
	  begin
	    Res      = compile(Code),
	    Expected = {ok, ?TEST_MODULE},
	    ?WHENFAIL(
	       begin
		 eqc:format("~s\n", [Code]),
		 compile(Code, [report_errors])
	       end,
	       equals(Res, Expected))
	  end).

prop_precompile() ->
  ?FORALL(Code, eqc_erlang_program:module(?TEST_MODULE),
	  begin
	    Res      = compile(Code, ['P']),
	    Expected = {ok,[]},
	    ?WHENFAIL(
	       begin
		 eqc:format("~s\n", [Code]),  %% normal compilation
		 compile(Code, [report_errors])
	       end,
	       equals(Res, Expected))
	  end).

