%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2017 15:51
%%%-------------------------------------------------------------------
-module(barrel_stats).
-author("benoitc").

%% API
-export([
  record_count/1, record_count/2, record_count/3,
  set_count/3,
  get_count/2,
  measure_time/3,
  timeit/3, timeit/4, timeit/5
]).


-export([start_link/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,  code_change/3]).

record_count(Name) ->
  barre_stats_counter:record(Name, #{}, 1).

record_count(Name, Labels) ->
  barre_stats_counter:record(Name, Labels, 1).

record_count(Name, Labels, Val) ->
  barre_stats_counter:record(Name, Labels, Val).


set_count(Name, Labels, Val) ->
  barre_stats_counter:set(Name, Labels, Val).

get_count(Name, Labels) ->
  barrel_stats_counter:value(Name, Labels).


measure_time(Name, Labels, Value) ->
  barrel_stats_histogram:set(Name, Labels, Value).

timeit(Name, Labels, F) ->
  T1 = erlang:monotonic_time(),
  Val = F(),
  T2 = erlang:monotonic_time(),
  Time = erlang:convert_time_unit(T2 - T1, native, second),
  measure_time(Name, Time, Labels),
  Val.

timeit(Name, Labels, F, A) ->
  T1 = erlang:monotonic_time(),
  Val = apply(F, A),
  T2 = erlang:monotonic_time(),
  Time = erlang:convert_time_unit(T2 - T1, native, second),
  measure_time(Name, Time, Labels),
  Val.

timeit(Name, Labels, M, F, A) ->
  T1 = erlang:monotonic_time(),
  Val = apply(M, F, A),
  T2 = erlang:monotonic_time(),
  Time = erlang:convert_time_unit(T2 - T1, native, second),
  measure_time(Name, Time, Labels),
  Val.


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  _ = ets:new(?MODULE,  [orderd_set, public, named_table, {read_concurrency, true}]),
  {ok, []}.

handle_call(Req, _From, State) ->
  lager:error("Unhandled call: ~p", [Req]),
  {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
  lager:error("Unhandled cast: ~p", [Msg]),
  {stop, {unhandled_cast, Msg}, State}.

handle_info(Info, State) ->
  lager:error("Unhandled info: ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
