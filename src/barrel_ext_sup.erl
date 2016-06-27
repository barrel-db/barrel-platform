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

%%%-------------------------------------------------------------------
%% @doc barrel supervisor for store backends & plugins
%% @end
%%%-------------------------------------------------------------------

-module(barrel_ext_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_proc/4, start_proc/5]).
-export([stop_proc/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN, 120000).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_proc(Name, M, F, A) ->
    start_proc(Name, M, F, A, []).

start_proc(Name, M, F, A, Opts) ->
	[Restart, Shutdown, Type, Modules] =
		[proplists:get_value(K, Opts, Default)
			|| {K, Default} <- [{restart, transient},
			{shutdown, ?SHUTDOWN},
			{type, worker},
			{modules, [M]}]],
	case supervisor:start_child(
		?MODULE, {Name, {M,F,A}, Restart, Shutdown, Type, Modules}) of
		{error, already_present} ->
			supervisor:restart_child(?MODULE, Name);
		Other ->
			Other
	end.


stop_proc(Name) ->
	supervisor:terminate_child(?MODULE, Name).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, {{one_for_one, 4, 3600}, []}}.
