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


-module(barrel_event).
-author("Benoit Chesneau").

-export([
  broadcast/2,
  subscribe/2,
  unsubscribe/2
]).


-type topic() ::
  { db, barrel_db:store(), all} |
  { db, barrel_db:store(), barrel:dbname() }.

-type event() :: any().


-spec broadcast(Topic::topic(), Event::event()) -> ok.
broadcast(Topic, Event) ->
  Service = gossip_service(),
  Service:broadcast(Topic, Event).


-spec subscribe(ClientPid::pid(), Topic::topic()) -> ok.
subscribe(Pid, {db, Store, all}) ->
  barrel_event_local:subscribe(Pid, {db, Store, '*'});
subscribe(Pid, {db, Store, DbName}) ->
  barrel_event_local:subscribe(Pid, {db, Store, DbName});
subscribe(_Pid, What) ->
  {error, {badarg, What}}.

-spec unsubscribe(Pid::pid(), Topic::topic()) -> ok.
unsubscribe(Pid, {db, Store, all}) ->
  barrel_event_local:unsubscribe(Pid, {db, Store, '*'});
unsubscribe(Pid, {db, Store, DbName}) ->
  barrel_event_local:unsubscribe(Pid, {db, Store, DbName});
unsubscribe(_Pid, What) ->
  {error, {badarg, What}}.

gossip_service() ->
  application:get_env(barrel, barrel_event_adapter, barrel_event_pg2).



