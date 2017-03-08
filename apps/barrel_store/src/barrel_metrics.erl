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
-behaviour(gen_server).

%% API

-export([ init/2
        , increment/1
        ]).

%% gen_server callbacks
-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

init(Type, Name) ->
  gen_server:cast(?MODULE, {init, Type, Name}).

increment(Name) ->
  gen_server:cast(?MODULE, {increment, Name}).


%% =============================================================================
%% gen_server API
%% =============================================================================

-record(state, {plugin}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

init([]) ->
  PluginModule = application:get_env(barrel_store, metrics_plugin, undefined),
  {ok, #state{plugin=PluginModule}}.

handle_cast(_, #state{plugin=undefined}=State) ->
  {noreply, State};

handle_cast({init, Type, Name}, #state{plugin=Plugin}=State) ->
  Plugin:init(Type, Name),
  {noreply, State};

handle_cast({increment, Name}, #state{plugin=Plugin}=State) ->
  Plugin:increment(Name),
  {noreply, State}.

handle_info(_Req, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
