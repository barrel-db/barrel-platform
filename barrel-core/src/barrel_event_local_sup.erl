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


-module(barrel_event_local_sup).
-author("Benoit Chesneau").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callback
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  NumShards = barrel_event_local:poolsize(),
  Shards = lists:map(
    fun(I) -> shard_spec(I) end,
    lists:seq(1, NumShards)),
  {ok, {{one_for_one, 1, 5}, Shards}}.

shard_spec(I) ->
  Name = barrel_event_local:shard_name(I),
  #{id => Name,
    start => {barrel_event_local, start_link, [Name]},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [barrel_event_local]
  }.
