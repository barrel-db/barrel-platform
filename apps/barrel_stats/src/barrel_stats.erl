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
  record_count/1, record_count/2,
  set_count/2, set_count/3,
  get_count/1, get_count/2,
  reset_count/2,
  measure_time/2, measure_time/3,
  get_measure_time/1, get_measure_time/2,
  reset_measure_time/2,
  timeit/3, timeit/4, timeit/5,
  register_metric/1,
  unregister_metric/1,
  list_metrics/0,
  reset_metrics/0,
  refresh/0,
  set_update_interval/1
]).

-export([start_link/0]).


-type metric() ::
  #{ type := metric_type(),
     name := metric_name(),
     help := metric_help(),
     mod => atom() }.
-type metric_name() :: list() | atom() | binary().
-type metric_type() ::  counter | histogram.
-type metric_help() :: string() | binary().

-export_type([
  metric/0,
  metric_name/0,
  metric_type/0,
  metric_help/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,  code_change/3]).

-define(STATS, barrel_stats).
-define(STATS_CACHE, barrel_stats_cache).

-define(UPDATE_INTERVAL_MS, 10000). % 10s

record_count(Name) ->
  barrel_stats_counter:record(Name, #{}).

record_count(Name, Labels) ->
  barrel_stats_counter:record(Name, Labels).

set_count(Name, Val) ->
  set_count(Name, #{}, Val).

set_count(Name, Labels, Val) ->
  barrel_stats_counter:set(Name, Labels, Val).

get_count(Name) -> get_count(Name, #{}).

get_count(Name, Labels) ->
  get_stat(Name, Labels, counter).

reset_count(Name, Labels) ->
  barrel_stats_counter:reset(Name, Labels).

measure_time(Name, Value) ->
  barrel_stats_histogram:set(Name, #{}, Value).

measure_time(Name, Labels, Value) ->
  barrel_stats_histogram:set(Name, Labels, Value).

get_measure_time(Name) -> get_measure_time(Name, #{}).

get_measure_time(Name, Labels) ->
  get_stat(Name, Labels, histogram).

reset_measure_time(Name, Labels) ->
  barrel_stats_histogram:reset(Name, Labels).

get_stat(Name, Labels, Type) ->
  Key = {{Name, Labels}, Type},
  
  case ets:lookup(?STATS, Key) of
    [] -> undefined;
    [{Key, Val}] ->
      case Type of
        counter -> Val;
        histogram ->
          Datapoints = [min, max, mean, 50, 75, 90, 95, 99, 999],
          barrel_stats_histogram:get_histogram(Val, Datapoints)
        
      end
  end.

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

%% @doc register a metric or a list of metrics.
-spec register_metric(Metric) -> Result when
  Metric :: metric(),
  Result :: ok | {error, already_exists} | {error, bad_metric}.
register_metric(Spec) ->
  gen_server:call(?MODULE, {register_metric, Spec}).

%% @doc unregister a metric or a collector
-spec unregister_metric(Name :: metric_name()) -> ok.
unregister_metric(Name) ->
  gen_server:call(?MODULE, {unregister_metric, Name}).

%% @doc lista all registered metric
-spec list_metrics() -> [metric()].
list_metrics() ->
  [M || {_, M} <- ets:tab2list(?STATS_CACHE)].

%% @doc reset all metrics.
-spec reset_metrics() -> ok.
reset_metrics() ->
  _ = lager:info("barrel_stats: reset all metrics", []),
  ok = barrel_stats_counter:reset_all(),
  ok = barrel_stats_histogram:reset_all(),
  true = ets:delete_all_objects(?STATS_CACHE),
  true = ets:delete_all_objects(?STATS),
  ok.

%% @doc force refreshing of the metrics
-spec refresh() -> ok.
refresh() ->
  gen_server:call(?MODULE, refresh).

%% @doc update interval in which the metrics will be extracted
-spec set_update_interval(Interval :: non_neg_integer()) -> ok.
set_update_interval(IntervalMs) ->
  gen_server:call(?MODULE, {set_interval, IntervalMs}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  _ = ets:new(?STATS, [ordered_set, public, named_table, {read_concurrency, true}]),
  _ = ets:new(?STATS_CACHE, [set, public, named_table, {read_concurrency, true}]),
  UpdateIntervalMs = application:get_env(barrel_stats, metric_update_interval_ms, ?UPDATE_INTERVAL_MS),
  TRef = erlang:send_after(UpdateIntervalMs, self(), tick),
  
  StartTime = erlang:monotonic_time(),
  InitState =
    #{
      collectors => #{},
      start_time => StartTime,
      last_tick_time => StartTime,
      ticker => TRef,
      update_interval_ms => UpdateIntervalMs
      },
  {ok, InitState}.


handle_call({register_metric, Spec}, _From, State) ->
  {Reply, State2} = do_declare_metric(Spec, State),
  {reply, Reply, State2};

handle_call({unregister_metric, Name}, _From, State) ->
  State2 = do_unregister_metric(Name, State),
  {reply, ok, State2};

handle_call(refresh, _From, State) ->
  ok = get_metrics(),
  {reply, ok, State};


handle_call({set_interval, IntervalMs}, _From, State) ->
  #{ ticker := TRef } = State,
  _ = erlang:cancel_timer(TRef),
  TRef2 = erlang:send_after(IntervalMs, self(), tick),
  {reply, ok, State#{ ticker => TRef2, update_interval_ms => IntervalMs }};

handle_call(Req, _From, State) ->
  _ = lager:error("Unhandled call: ~p", [Req]),
  {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
  _ = lager:error("Unhandled cast: ~p", [Msg]),
  {stop, {unhandled_cast, Msg}, State}.

handle_info(tick, State = #{ update_interval_ms := IntervalMs }) ->
  State2 = tick(State),
  TRef = erlang:send_after(IntervalMs, self(), tick),
  {noreply, State2#{ ticker => TRef }};

handle_info(Info, State) ->
  _ = lager:error("Unhandled info: ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%
%% Ticker
%%

tick(#{ last_tick_time := LastTickTime } = State) ->
  Now = erlang:monotonic_time(),
  TimeSince = erlang:convert_time_unit(Now - LastTickTime, native, millisecond),
  case TimeSince of
    0 ->
      State;
    _ ->
      ok = get_metrics(),
      %State1 = aggregate_metrics(State),
      %ok = report_metrics(State1),
      State#{ last_tick_time => Now }
  end.



get_metrics() ->
  Metrics = ets:tab2list(?STATS_CACHE),
  lists:foreach(
    fun({Name, #{ type := Type }}) ->
      case Type of
        counter ->
          Counters = barrel_stats_counter:values(Name),
          lists:foreach(
            fun({{_Name, Labels}, Value}) ->
              _ = barrel_stats_counter:set(Name, Labels, -Value),
              try
                ets:update_counter(?STATS, {{Name, Labels}, Type}, {2, Value})
              catch
                error:badarg ->
                  ets:insert_new(?STATS, {{{Name, Labels}, Type}, Value})
              end
            end, Counters);
        histogram ->
          Hists = barrel_stats_histogram:get_and_remove_raw_data(Name),
          lists:foreach(
            fun({_Name, Labels, Bin}) ->
              ets:insert(?STATS, {{{Name, Labels}, Type}, Bin})
            end, Hists)
      end
    end, Metrics),
  ok.
  


%%
%% Metric declaration
%%


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
  _ = ets:insert_new(?STATS, {{{Name, #{}}, counter}, 0}),
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

do_unregister_metric(Name, State) ->
  case ets:take(?STATS_CACHE, Name) of
    [{Name, #{ type := counter }}] ->
      _ = barrel_stats_counter:reset(Name);
    [] -> ok
  end,
  State.