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

-module(barrel).
-author("benoitc").

%% API
-export([all_databases/0]).


all_databases() ->
  {ok, Stores} = application:get_env(barrel, stores),
  AllDbs = lists:foldl(
    fun({Store, _, _}, Acc) ->
      Dbs = barrel_local_store:all_dbs(Store),
      Dbs ++ Acc
    end, [], Stores),
  lists:usort(AllDbs).
