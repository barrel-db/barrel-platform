%% Copyright 2016, Benoit Chesneau
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


-module(barrel_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  init_config(),
  Config = barrel_config:section_to_opts("api"),
  WebProcesses = web_processes(barrel_api_http:get_listeners(Config), Config),
   {ok, {{one_for_one, 10, 10}, WebProcesses}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
web_processes([], _Config) -> [];
web_processes(Listeners, Config) ->
  lists:flatten([ web_listeners(barrel_lib:propmerge1(Opts, Config), Scheme, Binding) ||
    {Scheme, Binding, Opts} <- Listeners ]).

web_listeners(Config, Scheme, Binding) -> barrel_api_http:binding_spec(Config, Scheme, Binding).

init_config() ->
  barrel_cors_middleware:init_config(),
  ok.


