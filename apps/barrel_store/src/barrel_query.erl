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

-module(barrel_query).

-export([query/6]).

-include("barrel_store.hrl").


-define(DEFAULT_MAX, 10000).


query(Db, Path, Fun, Acc, Type, Opts) ->
  _ = lager:info(
        "query(~p,~p,~p,~p,~p,~p)",
        [Db, Path, Fun, Acc, Type, Opts]
       ),
  Acc.
