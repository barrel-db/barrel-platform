-module(multiple_hosts_servers_example).

-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).
-export([start_phase/3]).

%% application
%% @doc Starts the application
start() ->
  application:ensure_all_started(multiple_hosts_servers_example).

%% @doc Stops the application
stop() ->
  application:stop(multiple_hosts_servers_example).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
  _ = application:stop(lager),
  ok = application:stop(sasl),
  {ok, _} = application:ensure_all_started(sasl),
  {ok, self()}.

%% @private
stop(_State) ->
  ok = cowboy:stop_listener(multiple_hosts_servers_http).

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, term()}.
start_phase(start_multiple_hosts_servers_example_http, _StartType, []) ->
  %% Host1
  {ok, #{hosts := [HostMatch11, HostMatch12], port := Port1}} =
    application:get_env(multiple_hosts_servers_example, api1),
  {ok, #{hosts := ['_'], port := Port2}} =
    application:get_env(multiple_hosts_servers_example, api2),
  {ok, ListenerCount} =
    application:get_env(multiple_hosts_servers_example, http_listener_count),

  Trails11 =
    trails:trails(example_echo_handler) ++
    cowboy_swagger_handler:trails(#{server => api1, host => HostMatch11}),
  Trails12 =
    trails:trails(host1_handler) ++
    cowboy_swagger_handler:trails(#{server => api1, host => HostMatch12}),
  Routes1 = [{HostMatch11, Trails11}, {HostMatch12, Trails12}],

  trails:store(api1, Routes1),
  Dispatch1 = trails:compile(Routes1),
  {ok, _} = start_cowboy(api1, ListenerCount, Dispatch1, Port1),

  Trails21 =
    trails:trails([host1_handler, example_echo_handler]) ++
    cowboy_swagger_handler:trails(#{server => api2}),

  trails:store(api2, Trails21),
  Dispatch2 = trails:single_host_compile(Trails21),
  {ok, _} = start_cowboy(api2, ListenerCount, Dispatch2, Port2),
  ok.

%% @private
start_cowboy(Server, ListenerCount, Dispatch, Port) ->
  RanchOptions = [{port, Port}],
  CowboyOptions =
    [{env, [{dispatch, Dispatch}]}, {compress, true}, {timeout, 12000}],
  cowboy:start_http(Server, ListenerCount, RanchOptions, CowboyOptions).
