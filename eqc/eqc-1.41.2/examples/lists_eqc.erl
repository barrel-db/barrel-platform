-file("eqc-1.41.2/examples/lists_eqc.erl", 0).
%%% @author Thomas Arts <thomas@ThomasComputer.local>
%%% @copyright (C) 2016, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created :  2 Aug 2016 by Thomas Arts <thomas@ThomasComputer.local>

-module(lists_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

seq(M,N) ->
  lists:seq(M,N).


prop_seq_length() ->
  ?FORALL({M,N}, {int(), int()},
          if N == M-1 -> seq(M,N) == [];
             N > M-1  -> seq(M,N) == seq(M, N-1) ++ [N];
             N < M-1 -> is_exit(catch seq(M,N))
          end).

is_exit({'EXIT',_}) -> true;
is_exit(_)          -> false. 
  

