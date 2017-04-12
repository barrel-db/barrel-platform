%% Copyright 2017, Bernard Notarianni
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_stats_statsd).
-author("Bernard Notarianni").

-behaviour(barrel_stats_plugin).
-bahaviour(gen_server).

%% plugin callbacks
-export([ start/0
        , init/3
        , increment/3
        , set_value/3
        , duration/3
        ]).

%% gen_server callbacks

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).


-record(state, {socket, key}).

%% Specification for statsd message format:
%% https://github.com/etsy/statsd/blob/master/docs/metric_types.md


start() ->
  start_link().

init(_Type, _Name, _Env) ->
  ok.

set_value(Name, Value, _Env) ->
  push(Name, {gauge, Value}),
  ok.

increment(Name, Value, _Env) ->
  push(Name, {counter, Value}),
  ok.

duration(Name, Value, _Env) ->
  push(Name, {duration, Value}),
  ok.

push(MetricName, Value) ->
  gen_server:cast(?MODULE, {send, MetricName, Value}).


%% gen_server api

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% Server functions
init(_) ->
  Env = application:get_env(barrel_store, metrics, undefined),
  {Peer, Port} = proplists:get_value(statsd_server, Env),
  [Node, Host] = binary:split(atom_to_binary(node(), utf8), <<"@">>),
  HostWithoutDots = barrel_lib:binary_join(binary:split(Host, <<".">>, [global]), <<"_">>),
  StatsdKey = barrel_lib:binary_join([<<"barrel">>, HostWithoutDots, Node], <<".">>),
  {ok, Socket} = gen_udp:open(0),
  _ = lager:info("init statsd metrics peer=~p port=~p", [Peer, Port]),
  {ok, #state{socket={Socket, Peer, Port}, key=StatsdKey}}.

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({send, MetricName, Value}, #state{socket=Socket, key=ServerKey}=State) ->
  MetricJoined = barrel_lib:binary_join(MetricName, <<".">>),
  FullKey = barrel_lib:binary_join([ServerKey, MetricJoined], <<".">>),
  send(Socket, FullKey, Value),
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(normal, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send(Socket, Key, {counter, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|c">>,
  udp(Socket, Data);

send(Socket, Key, {gauge, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|g">>,
  udp(Socket, Data);

send(Socket, Key, {duration, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|ms">>,
  udp(Socket, Data).

udp({Socket, Peer, Port}, Data) ->
  _ = gen_udp:send(Socket, Peer, Port, Data),
  ok.
