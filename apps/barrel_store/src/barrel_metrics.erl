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

-module(barrel_metrics).
-author("Bernard Notarianni").

%% API

-export([ start/0
        , init/2
        , increment/1
        , increment/2
        , set_value/2
        , duration/2
        , duration_since/2
        ]).

start() ->
  lager:info("start"),
  Env = application:get_env(barrel_store, metrics, undefined),
  case Env of
    undefined -> ok;
    Env ->
      Plugin = proplists:get_value(plugin, Env),
      Plugin:start()
  end.

init(Type, Name) ->
  Env = application:get_env(barrel_store, plugin, undefined),
  case Env of
    undefined -> ok;
    Env ->
      Plugin = proplists:get_value(plugin, Env),
      Plugin:init(Type, Name, Env)
  end.

increment(Name) ->
  increment(Name, 1).
increment(Name, Value) ->
  Env = application:get_env(barrel_store, metrics, undefined),
  case Env of
    undefined -> ok;
    Env ->
      Plugin = proplists:get_value(plugin, Env),
      Plugin:increment(Name, Value, Env)
  end.

set_value(Name, Value) ->
  Env = application:get_env(barrel_store, metrics, undefined),
  case Env of
    undefined -> ok;
    Env ->
      Plugin = proplists:get_value(plugin, Env),
      Plugin:set_value(Name, Value, Env)
  end.

duration_since(Name, StartTime) ->
  Duration = trunc(timer:now_diff(os:timestamp(), StartTime) / 1000),
  duration(Name, Duration).

duration(Name, Value) ->
  Env = application:get_env(barrel_store, metrics, undefined),
  case Env of
    undefined -> ok;
    Env ->
      Plugin = proplists:get_value(plugin, Env),
      Plugin:duration(Name, Value, Env)
  end.
