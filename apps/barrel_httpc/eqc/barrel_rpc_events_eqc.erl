%%% @author Zachary Kessin <>
%%% @copyright (C) 2017, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2017 by Zachary Kessin <>

-module(barrel_rpc_events_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).
-define(DB, <<"testdb8">>).
%% -- State and state functions ----------------------------------------------
-record(state,{
          keys:: dict:dict(binary(), term()),
          online = true :: boolean(),
					ids
              }).


clean_ids(Ids) ->
		[barrel:delete(?DB, Id, #{}) || Id <- Ids].

%% @doc Returns the state in which each test case starts. (Unless a different
%%      initial state is supplied explicitly to, e.g. commands/2.)
-spec initial_state() -> eqc_statem:symbolic_state().
initial_state() ->
		Ids = vector(256, utf8(16)),

    #state{keys = dict:new(), ids= Ids}.

db() ->
    ?DB.


id(Ids) ->
		oneof(Ids).


doc(Ids)->
    #{
		 <<"id">> => id(Ids),
		 <<"content">> => utf8(8)
		 }.




%********************************************************************************
% Validate the results of a call

get_command(#state{keys = Dict, ids= Ids}) ->
    oneof([
           {call, barrel, get,       [db(), oneof(dict:fetch_keys(Dict)), #{}]},
           {call, barrel, get,       [db(), id(Ids), #{}]}
          ]).


get_pre(#state{keys = Dict}) ->
    not(dict:is_empty(Dict)).


get_post(#state{keys= Dict}, [_DB, Id, #{}], {error, not_found}) ->

    not(dict:is_key(Id, Dict));
get_post(#state{keys= Dict}, [_DB, Id, #{}],
         {ok, Doc = #{<<"id">> := Id, <<"content">> := _Content} ,
          _rev}) ->
    {ok, Doc} == dict:find(Id, Dict).


%********************************************************************************
post_pre(_S) ->
    true.


post_post(#state{keys = Dict} , [_DB, #{<<"id">> := Id}, #{}], {error, {conflict, doc_exists}}) ->
		dict:is_key(Id, Dict);

post_post(_State, _Args, _Ret) ->
    true.

post_command(#state{keys = Dict, ids = Ids}) ->

		case dict:is_empty(Dict) of
        true ->
            oneof([{call, barrel, post,  [db(), doc(Ids), #{}]}]);
        false ->
            oneof([
                   {call, barrel, post,  [db(), doc(Ids), #{}]},
									 {call, barrel, put,   [db(), doc(Ids), #{}]},
									 {call, barrel, post,  [db(), update_doc(Dict), #{}]},
                   {call, barrel, put,   [db(), update_doc(Dict), #{}]}
                  ]
                 )
		end.




post_next(State = #state{keys = Dict},_V,[DB, Doc = #{<<"id">> := Id} |_]) ->
    case dict:is_key(Id, Dict) of
        true ->
            State#state {keys = dict:store(Id, Doc, Dict)};
        false ->
						barrel:delete(DB, Id, #{}),
            State#state {keys = dict:store(Id, Doc, Dict)}
        end.


%********************************************************************************


delete_pre(#state{keys = Dict}) ->
    not(dict:is_empty(Dict)).

delete_post(#state{keys= Dict},[_DB, Id,_] , {error,not_found}) ->
    not(dict:is_key(Id, Dict));
delete_post(#state{keys= Dict}, [_DB, Id, #{}], {ok, Id, _rev}) ->
    dict:is_key(Id, Dict).

delete_command(#state{keys = Dict, ids= Ids}) ->
    oneof([
           {call, barrel, delete,    [db(), oneof(dict:fetch_keys(Dict)), #{}]},
           {call, barrel, delete,    [db(), id(Ids), #{}]}
          ]).


delete_next(State = #state{keys = Dict},_V,[_DB, Id|_]) ->
    State#state{keys = dict:erase(Id, Dict)}.


update_doc(Dict) ->
    ?LET({Key, NewContent},
         {oneof(dict:fetch_keys(Dict)), utf8(9)},
         begin
             {ok, Doc1} = dict:find(Key, Dict),
             Doc1#{<<"content">> => NewContent}

         end).




%% offline_pre(#state{online= Online}) ->
%%      not(Online).

%% offline_command(_S) ->
%%       [].

%% -- Generators -------------------------------------------------------------

%% -- Common pre-/post-conditions --------------------------------------------
%% @doc General command filter, checked before a command is generated.
-spec command_precondition_common(S, Cmd) -> boolean()
    when S    :: eqc_statem:symbolic_state(),
         Cmd  :: atom().
command_precondition_common(_S, _Cmd) ->
    true.

%% @doc General precondition, applied *before* specialized preconditions.
-spec precondition_common(S, Call) -> boolean()
    when S    :: eqc_statem:symbolic_state(),
         Call :: eqc_statem:call().
precondition_common(_S, _Call) ->
    true.

%% @doc General postcondition, applied *after* specialized postconditions.
-spec postcondition_common(S, Call, Res) -> true | term()
    when S    :: eqc_statem:dynamic_state(),
         Call :: eqc_statem:call(),
         Res  :: term().
postcondition_common(_S, _Call, _Res) ->
    true.

%% -- Operations -------------------------------------------------------------



%% --- ... more operations

%% -- Property ---------------------------------------------------------------
%% @doc Default generated property


init_db()->
    {ok, _} = application:ensure_all_started(barrel_rest),
		case barrel:create_database(#{ <<"database_id">> => ?DB }) of
				{error, db_exists} -> ok;
				{ok, #{<<"database_id">> := ?DB}}-> ok
		end,

    fun delete_db/0.

delete_db() ->
    ok = barrel:delete_database(?DB),
    ok.


-spec prop_barrel_rpc_events_eqc() -> eqc:property().
prop_barrel_rpc_events_eqc() ->
    ?SETUP(fun init_db/0,
           ?FORALL(Cmds, commands(?MODULE),
                   begin

                       {H, S, Res} = run_commands(Cmds),

											 ?WHENFAIL(begin
																		 io:format("~n~n~n Keys : ~p ",[dict:fetch_keys(S#state.keys)])
																 end,

																 check_command_names(Cmds,
																										 measure(length, commands_length(Cmds),
																														 pretty_commands(?MODULE, Cmds, {H, S, Res},
																																						 Res == ok))))
                   end)).

%% @doc Run property repeatedly to find as many different bugs as
%% possible. Runs for 10 seconds before giving up finding more bugs.
-spec bugs() -> [eqc_statem:bug()].
bugs() -> bugs(10).

%% @doc Run property repeatedly to find as many different bugs as
%% possible. Runs for N seconds before giving up finding more bugs.
-spec bugs(non_neg_integer()) -> [eqc_statem:bug()].
bugs(N) -> bugs(N, []).

%% @doc Run property repeatedly to find as many different bugs as
%% possible. Takes testing time and already found bugs as arguments.
-spec bugs(non_neg_integer(), [eqc_statem:bug()]) -> [eqc_statem:bug()].
bugs(Time, Bugs) ->
    more_bugs(eqc:testing_time(Time, prop_barrel_rpc_events_eqc()), 20, Bugs).
