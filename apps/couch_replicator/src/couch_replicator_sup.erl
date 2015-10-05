
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_replicator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    catch exit(whereis(couch_replicator_sup), normal).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
        {couch_replication_event,
            {gen_event, start_link, [{local, couch_replication}]},
            permanent,
            brutal_kill,
            worker,
            dynamic},
        {couch_replicator_job_sup,
            {couch_replicator_job_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_replicator_job_sup]},
        {couch_replicator_manager_sup,
            {couch_replicator_manager_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_replicator_manager_sup]}
    ],
    {ok, { {one_for_one, 100, 3600}, Children} }.
