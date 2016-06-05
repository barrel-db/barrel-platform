%% Copyright 2016, Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://joinup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

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
  Config = application:get_env(barrel, api, []),
  WebProcesses = web_processes(barrel_api_http:get_listeners(Config), Config),
   {ok, {{one_for_one, 10, 10}, WebProcesses}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
web_processes([], _Config) -> [];
web_processes(Listeners, Config) ->
  lists:flatten([ web_listeners(Config, Scheme, Binding) ||  {Scheme, Binding} <- Listeners ]).

web_listeners(Config, Scheme, Binding) -> barrel_api_http:binding_spec(Config, Scheme, Binding).
