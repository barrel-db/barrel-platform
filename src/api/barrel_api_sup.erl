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
  Config = init_config(),
  WebProcesses = web_processes(barrel_api_http:get_listeners(Config), Config),
   {ok, {{one_for_one, 10, 10}, WebProcesses}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

web_processes(Listeners, Config) ->
  maps:fold(fun(Scheme, {Binding, Opts}, Specs) ->
        [web_listeners(barrel_lib:propmerge1(Opts, Config), Scheme, Binding) | Specs]
      end, [], Listeners).

web_listeners(Config, Scheme, Binding) -> barrel_api_http:binding_spec(Config, Scheme, Binding).

%% TODO: move the CORS config initialisation to a config hook
init_config() ->
  barrel_cors_middleware:init_config(),

  %% environment config is always superseded by the ini config.
  EnvConfig = application:get_env(barrel, api, []),
  IniConfig = barrel_config:section_to_opts("api"),
  barrel_lib:propmerge1(IniConfig, EnvConfig).


