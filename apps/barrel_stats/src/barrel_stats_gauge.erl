%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2017 11:58
%%%-------------------------------------------------------------------
-module(barrel_stats_gauge).
-author("benoitc").
-behaviour(gen_server).

%% API
-export([
  create/1,
  notify/2,
  get_value/1,
  take_value/1
]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

create(Name) ->
  notify(Name, 0).

notify(Name, Value) ->
  ets:insert(?MODULE, {Name, Value}),
  ok.

get_value(Name) ->
  case ets:lookup(?MODULE, Name) of
    [{_, R}] -> R;
    [] -> erlang:error(not_found)
  end.

take_value(Name) ->
  case ets:take(?MODULE, Name) of
    [{_, R}] -> R;
    [] -> erlang:error(not_found)
  end.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  _ = ets:new(?MODULE, [named_table, set, public]),
  {ok, #{}}.

handle_call(_Request, _From, State) ->  {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.