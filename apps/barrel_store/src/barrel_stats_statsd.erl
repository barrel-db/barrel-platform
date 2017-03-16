%% Copyright 2016, Bernard Notarianni
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
-export([ init/3
        , increment/3
        , set_value/3
        , duration/3
        ]).

%% Specification for statsd message format:
%% https://github.com/etsy/statsd/blob/master/docs/metric_types.md

init(_Type, _Name, _Env) ->
  ok.

set_value(Name, Value, Env) ->
  Server = proplists:get_value(statsd_server, Env),
  push(Server, Name, {gauge, Value}),
  ok.

increment(Name, Value, Env) ->
  Server = proplists:get_value(statsd_server, Env),
  push(Server, Name, {counter, Value}),
  ok.

duration(Name, Value, Env) ->
  Server = proplists:get_value(statsd_server, Env),
  push(Server, Name, {duration, Value}),
  ok.

push(_, _, undefined) ->
  ok;
push(Server, Name, Value) ->
  [_, NodeId] = binary:split(atom_to_binary(node(), utf8), <<"@">>),
  [Node | _] = binary:split(NodeId, <<".">>),
  StasdKey = barrel_lib:binary_join(Name, <<"/">>),
  NameWithNode = <<Node/binary, ".", StasdKey/binary>>,
  send(Server, NameWithNode, Value).

send({Peer, Port}, Key, {counter, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|c">>,
  udp(Peer, Port, Data);

send({Peer, Port}, Key, {gauge, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|g">>,
  udp(Peer, Port, Data);

send({Peer, Port}, Key, {duration, Value}) ->
  BVal = integer_to_binary(Value),
  Data = <<Key/binary, ":", BVal/binary, "|ms">>,
  udp(Peer, Port, Data).

udp(Peer, Port, Data) ->
  Fun = fun() ->
            case gen_udp:open(0) of
              {ok, Socket} ->
                gen_udp:send(Socket, Peer, Port, Data),
                gen_udp:close(Socket);
              Error ->
                lagger:error("can not open udp socket to statsd server: ~p", [Error])
            end
        end,
  spawn(Fun).
