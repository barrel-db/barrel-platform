-file("eqc-1.41.2/examples/putline_eqc.erl", 0).
%%% @author  <John@JOHNSTABLET2014>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2015 by  <John@JOHNSTABLET2014>

-module(putline_eqc).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-compile(export_all).

initial_state() ->
  undefined.

%% putline(S) ->
%%   [putchar:putchar(C) || C <- S],
%%   putchar:newline().

putline(S) ->
  putbuff:putbuff(S++"\n").

putline_args(_S) -> 
  [list(char())].

putline_callouts(_S, [String]) ->
  ?SEQ([?CALLOUT(putchar,putchar,[Char],ok)
        || Char <- String] ++
         [?CALLOUT(putchar,newline,[],ok)]).

putline_features(_,[S],_) ->
  putline.

api_spec() ->
  #api_spec{modules = 
              [#api_module{name = putchar,
                           functions =
                             [#api_fun{name = putchar,
                                       arity = 1,
                                       classify = putchar_eqc
                                      }] ++
                             [#api_fun{name = newline,
                                       arity = 0,
                                       classify = putchar_eqc
                                      }]}] }.

prop_putline() ->
  ?SETUP(fun() -> 
             eqc_mocking:start_mocking(api_spec()),  
             fun() -> ok end
         end, 
         ?FORALL(Cmds, commands(?MODULE),
                 begin
                   {H, S, Res} = run_commands(?MODULE,Cmds),
                   pretty_commands(?MODULE, Cmds, {H, S, Res},
                                   aggregate(eqc_statem:call_features(H),
                                             Res == ok))
                 end)).

