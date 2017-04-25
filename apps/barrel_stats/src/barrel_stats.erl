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
  reset_count/2,
  measure_time/3,
  reset_measure_time/2,
  timeit/3, timeit/4, timeit/5,
  register_metric/1,
  unregister_metric/1,
  list_metrics/0,
  reset_metrics/0
]).


-export([start_link/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,  code_change/3]).

-define(STATS, barrel_stats).
-define(STATS_CACHE, barrel_stats_cache).

record_count(Name) ->
  barrel_stats_counter:record(Name, #{}, 1).

record_count(Name, Labels) ->
  barrel_stats_counter:record(Name, Labels, 1).

record_count(Name, Labels, Val) ->
  barrel_stats_counter:record(Name, Labels, Val).

set_count(Name, Labels, Val) ->
  barrel_stats_counter:set(Name, Labels, Val).

get_count(Name, Labels) ->
  barrel_stats_counter:value(Name, Labels).

reset_count(Name, Labels) ->
  barrel_stats_counter:reset(Name, Labels).

reset_measure_time(Name, Labels) ->
  barrel_stats_histogram:reset(Name, Labels).

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

register_metric(Spec) ->
  gen_server:call(?MODULE, {register_metric, Spec}).

unregister_metric(Name) ->
  gen_server:call(?MODULE, {unregister_metric, Name}).


list_metrics() ->
  [M || {_, M} <- ets:tab2list(?STATS_CACHE)].

reset_metrics() ->
  lager:info("barrel_stats: reset all metrics", []),
  ok = barrel_stats_counter:reset_all(),
  ok = barrel_stats_histogram:reset_all(),
  true = ets:delete_all_objects(?STATS_CACHE),
  true = ets:delete_all_objects(?STATS),
  ok.


-spec start_link() -> pid().
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  _ = ets:new(?STATS, [ordered_set, public, named_table, {read_concurrency, true}]),
  _ = ets:new(?STATS_CACHE, [set, public, named_table, {read_concurrency, true}]),
  InitState = #{ collectors => #{} },
  {ok, InitState}.


handle_call({register_metric, Spec}, _From, State) ->
  {Reply, State2} = do_declare_metric(Spec, State),
  {reply, Reply, State2};

handle_call({unregister_metric, Name}, _From, State) ->
  State2 = do_unregister_metric(Name, State),
  {reply, ok, State2};

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

do_declare_metric(#{ name := default, type := collector }, _State) ->
  {error, already_exists};
do_declare_metric(#{ name := Name, type := collector, mod := Mod }, State = #{ collectors := Collectors }) ->
  case maps:is_key(Name, Collectors) of
    false ->
      Metrics = Mod:describe(),
      case add_metrics(Metrics) of
        ok ->
          _ = [maybe_init_counter(M) || M <- Metrics],
          Collectors2 = maps:put(Name, Mod, Collectors),
          {ok, State#{ collectors => Collectors2 }};
        Error ->
          {Error, State}
      end;
    true ->
      {{error, already_exists}, State}
  end;
do_declare_metric(Metric, State) when is_map(Metric)->
  case add_metric(Metric) of
    ok ->
      ok = maybe_init_counter(Metric),
      {ok, State};
    Error ->
      {Error, State}
  end;
do_declare_metric(Metrics, State) when is_list(Metrics) ->
  case add_metrics(Metrics) of
    ok ->
      _ = [maybe_init_counter(M) || M <- Metrics],
      {ok, State};
    Error ->
      {Error, State}
  end;
do_declare_metric(_, State) ->
  {{error, bad_metric}, State}.

maybe_init_counter(#{ name := Name, type := counter }) ->
  _ = barrel_stats_counter:set(Name, #{}, 0),
  ok;
maybe_init_counter(_) ->
  ok.

add_metrics(Metrics) -> add_metrics(Metrics, []).

add_metrics([ Metric | Rest], Records) ->
  case validate_metric(Metric) of
    {ok, Record} -> add_metrics(Rest, [Record | Records]);
    Error -> Error
  end;
add_metrics([], Records) ->
  _ = ets:insert(?STATS_CACHE, Records),
  ok.

validate_metric(#{ name := Name, type := Type, help := _Help}=Metric) ->
  case ets:lookup(?STATS_CACHE, Name) of
    [] ->
      case validate_type(Type) of
        ok -> {ok, {Name, Metric}};
        error -> {error, bad_metric}
      end;
    [{Name, _}] ->
      {error, already_exists}
  end;
validate_metric(_) ->
  {error, bad_metric}.

validate_type(counter) -> ok;
validate_type(histogram) -> ok;
validate_type(_) -> error.

add_metric(Metric) ->
  case validate_metric(Metric) of
    {ok, Record} ->
      _ = ets:insert(?STATS_CACHE, Record),
      ok;
    Error ->
      Error
  end.

do_unregister_metric(Name, State = #{ collectors := Collectors}) ->
  case maps:take(Name, Collectors) of
    {Mod, Collectors2} ->
      Metrics = Mod:describe(),
      _ = [ets:delete(?STATS_CACHE, N) || #{name := N} <- Metrics],
      State#{ collectors => Collectors2 };
    error ->
      _ = ets:delete(?STATS_CACHE, Name),
      State
  end.

