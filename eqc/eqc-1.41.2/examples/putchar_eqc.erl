-file("eqc-1.41.2/examples/putchar_eqc.erl", 0).
%%% @author  <John@JOHNSTABLET2014>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2015 by  <John@JOHNSTABLET2014>

-module(putchar_eqc).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-compile(export_all).

putchar_args(_S) -> 
  [frequency([{10,char()},{1,$\n}])].

putchar_pre(_S,[C]) ->
  C /= $\n.

%% putchar_callouts(S, [$\n]) ->
%%   ?CALLOUT(putbuff,putbuff,[S++"\n"],ok);
putchar_callouts(_S, [_Char]) ->
  ?EMPTY.

%% putchar_next(_S, _Value, [$\n]) ->
%%   [];
putchar_next(S, _Value, [Char]) ->
  S++[Char].

putchar_features(_,[C],_) ->
  putchar.
  %% case C of
  %%   $\n -> {putchar,newline};
  %%   _   -> {putchar,other}
  %% end.

putchar_callers() ->
  [putline_eqc].

newline_args(_S) -> 
  [].

newline_callouts(S, []) ->
  ?CALLOUT(putbuff,putbuff,[S++"\n"],ok).

newline_next(_S, _Value, []) ->
  [].

newline_features(_,[],_) ->
  newline.

newline_callers() ->
  [putline_eqc].


initial_state() ->
  [].


newline() ->
  putbuff:putbuff(get(buff)++"\n"),
  put(buff,[]).

putchar(C) ->
  put(buff,get(buff)++[C]).

api_spec() ->
  #api_spec{modules = 
              [#api_module{name = putbuff,
                           functions =
                             [#api_fun{name = putbuff,
                                       arity = 1
                                      }]}] }.


prop_putchar() ->
  ?SETUP(fun() -> 
             eqc_mocking:start_mocking(api_spec()),  
             fun() -> ok end
         end, 
         ?FORALL(Cmds, commands(?MODULE),
                 begin
                   put(buff,[]),
                   {H, S, Res} = run_commands(?MODULE,Cmds),
                   pretty_commands(?MODULE, Cmds, {H, S, Res},
                                   measure(length, length(Cmds),
                                           aggregate(eqc_statem:call_features(H),
                                                     Res == ok)))
                 end)).

