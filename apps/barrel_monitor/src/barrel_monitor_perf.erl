%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2017 09:20
%%%-------------------------------------------------------------------
-module(barrel_monitor_perf).
-author("benoitc").

%% API
-export([
  init_context/0
]).


%% Hooks
-export([
  barrel_start_transaction/2,
  barrel_end_transaction/2,
  barrel_http_in/1,
  barrel_http_out/3
]).


barrel_start_transaction(Trans, DbName) ->
  erlang:put(barrel_transaction_start_time, erlang:monotonic_time()),
  prometheus_counter:inc(barrel_db_transactions, [DbName, Trans]).

barrel_end_transaction(Trans, DbName) ->
  T1 = erlang:get(barrel_transaction_start_time),
  T2 = erlang:monotonic_time(),
  prometheus_histogram:observe(barrel_db_transaction_duration, [DbName, Trans], T2 - T1).

barrel_http_in(#{ method := Method }) ->
  prometheus_counter:inc(barrel_http_request_total, [Method]),
  prometheus_gauge:inc(barrel_http_requests).

barrel_http_out(_Req, StatusCode, Duration) ->
  prometheus_gauge:dec(barrel_http_requests),
  prometheus_counter:inc(barrel_http_response_total, [StatusCode]),
  prometheus_histogram:observe(barrel_http_request_duration, Duration).

init_metrics() ->
  %% transactions metrics

  ok = prometheus_counter:new([
    {name, barrel_db_transactions},
    {help, ""},
    {labels, [db, type]}]),

  ok = prometheus_histogram:new([
    {name, barrel_db_transaction_duration},
    {buckets, microseconds_duration_buckets()},
    {labels, [db, type]},
    {help, ""},
    {duration_unit, microseconds}]),

  %% HTTP metrics

  ok = prometheus_counter:new([
    {name, barrel_http_request_total},
    {help, ""},
    {labels, [method]}]),

  ok = prometheus_gauge:new([
    {name, barrel_http_requests},
    {help, ""}]),

  ok = prometheus_histogram:new([
    {name, barrel_http_request_duration},
    {buckets, microseconds_duration_buckets()},
    {help, ""},
    {duration_unit, microseconds}]),

  ok = prometheus_counter:new([
    {name, barrel_http_response_total},
    {help, ""},
    {labels, [status]}]),

  ok.

microseconds_duration_buckets() ->
  [10, 25, 50, 100, 250, 500,
    1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
    1000000, 2500000, 5000000, 10000000].

init_hooks() ->
  ok = hooks:reg(?MODULE, barrel_start_transaction, 2),
  ok = hooks:reg(?MODULE, barrel_end_transaction, 2),
  ok = hooks:reg(?MODULE, barrel_http_in, 1),
  ok = hooks:reg(?MODULE, barrel_http_out, 3),
  ok.

init_context() ->
  ok = init_metrics(),
  ok = init_hooks(),
  ok.