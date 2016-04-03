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

-module(couch_sup).
-behaviour(supervisor).


-export([start_link/0]).
-export([stop/0]).
-export([restart_core_server/0]).

-include("couch_db.hrl").

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    % announce startup
    io:format("couch ~s is starting.~n", [ couch_server:get_version() ]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

restart_core_server() ->
    init:restart().




stop() ->
    catch exit(whereis(couch_sup), normal).

init(_) ->
    Servers = [{couch_primary_services,
                {couch_primary_sup, start_link, []},
                    permanent,
                    infinity,
                    supervisor,
                    [couch_primary_sup]},
                {couch_secondary_services,
                    {couch_secondary_sup, start_link, []},
                    permanent,
                    infinity,
                    supervisor,
                    [couch_secondary_sup]}],

    {ok, {{one_for_all, 10, 3600}, Servers}}.
