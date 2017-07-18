-file("eqc-1.41.2/examples/reg_eqc_cpt.erl", 0).
-module(reg_eqc_cpt).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

%% -- State ------------------------------------------------------------------
-record(state, {pids = [], regs = []}).

initial_state() ->
  #state{}.

%% -- Generators -------------------------------------------------------------

-define(names, [a, b, c, d]).

name() ->
    elements(?names).

pid(S) ->
    elements(S#state.pids).

%% -- Operations -------------------------------------------------------------

%% --- Operation: spawn ---

spawn_args(_S) -> 
  [].

spawn() ->
  erlang:spawn(timer, sleep, [5000]).

spawn_next(S, Res, []) ->
  S#state{pids = S#state.pids ++ [Res]}.

%% --- Operation: register ---

register_pre(S) ->
  S#state.pids /= [].

register_args(S) ->
  [name(), pid(S)].

register(Name, Pid) ->
  catch erlang:register(Name, Pid).

register_next(S, _Res, [Name, Pid]) ->
  case register_ok(S, [Name, Pid]) of
    true ->
      S#state{regs = S#state.regs ++ [{Name, Pid}]};
    false ->
      S
  end.

register_post(S, [Name, Pid], Result) ->
  case register_ok(S, [Name, Pid]) of
    true ->
      eq(Result, true);
    false ->
      is_exit(Result)
  end.

register_ok(S, [Name, Pid]) ->
  not lists:keymember(Name, 1, S#state.regs) andalso
    not lists:keymember(Pid, 2, S#state.regs).

is_exit({'EXIT', _}) ->
  true;
is_exit(Other) ->
  {expected_EXIT, Other}.


%% --- Operation: unregister ---

unregister_pre(S, [Name]) ->
  lists:keymember(Name, 1, S#state.regs).

unregister_args(_) ->
  [name()].

unregister(Name) ->
  erlang:unregister(Name).

unregister_next(S, _, [Name]) ->
  S#state{regs = lists:keydelete(Name, 1, S#state.regs)}.

%% --- Operation: whereis ---

whereis_args(_) ->
  [name()].

whereis(Name) ->
  erlang:whereis(Name).

whereis_post(S, [Name], Result) ->
  case lists:keyfind(Name, 1, S#state.regs) of
    {Name, Pid} ->
      eq(Result, Pid);
    false ->
      eq(Result, undefined)
  end.

%% -- Property ---------------------------------------------------------------

weight(_S, spawn) -> 1;
weight(_S, _Cmd)  -> 2.

prop_reg_eqc() ->
  ?FORALL(Cmds, commands(?MODULE), 
  begin
    cleanup(), 
    {H, S, Res} = run_commands(?MODULE, Cmds), 
    pretty_commands(?MODULE, Cmds, {H, S, Res}, 
                    measure(length, length(Cmds), 
                            aggregate(command_names(Cmds), 
                                      Res == ok)))
  end).

cleanup() ->
    [catch erlang:unregister(Name) || Name <- ?names].

