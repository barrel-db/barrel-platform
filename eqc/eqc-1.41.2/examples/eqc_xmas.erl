-file("eqc-1.41.2/examples/eqc_xmas.erl", 0).
%%% @author Thomas Arts <thomas@ThomasComputer.local>
%%% @copyright (C) 2015, Thomas Arts
%%% @doc Christmas decoration for QuickCheck tests. 
%%%      Instead of dots and crosses, some jingle bells are shown on ascii terminals
%%% @end
%%% Created : 16 Dec 2015 by Thomas Arts <thomas@ThomasComputer.local>

-module(eqc_xmas).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

decorate(P) ->
  on_output(fun(".", []) ->
                io:format("~ts ", [<<240,159,142,132>>]);
               ("x", []) ->
                io:format("~ts ", [<<240,159,142,133>>]);
               (S,Args) ->
                io:format(S, Args)
            end, P).

prop_decorate() ->
  ?FORALL(X, int(),
     ?IMPLIES(X rem 7 /= 0, true)).



