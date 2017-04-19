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

-module(barrel_metrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  Env = application:get_env(barrel_store, metrics, undefined),
  case Env of
    undefined ->
      ignore;
    Env ->
      Mod = proplists:get_value(plugin, Env),
      lager:info("metric plugin = ~p",[Mod]),
      Spec = #{
        id => barrel_metrics,
        start => {Mod, start_link, []},
        restart => temporary,
        shutdown => 30000,
        type => worker,
        modules => [Mod]
       },
      {ok, {{simple_one_for_one, 4, 3600}, [Spec]}}
  end.
