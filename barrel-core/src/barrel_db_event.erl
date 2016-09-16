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

-module(barrel_db_event).
-author("benoitc").

%% API
-export([key/1]).
-export([notify/2]).

key(DbName) ->
    {n, l, {event, DbName}}.

notify(DbName, Event) ->
  Key = key(DbName),
  gen_event:notify({via, gproc, Key}, Event).
