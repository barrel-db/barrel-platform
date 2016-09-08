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

-module(barrel_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ok = have_default_store(),
  {ok, Sup} = barrel_sup:start_link(),
  init_dbs(),
  {ok, Sup}.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================


have_default_store() ->
  case application:get_env(barrel, default_store) of
    undefined ->
      erlang:error(no_default_store);
    {ok, Name} ->
      {ok, Stores} = application:get_env(barrel, stores),
      case lists:keymember(Name, 1, Stores) of
        true -> ok;
        false -> erlang:error({invalid_default_store, Name})
      end
  end,
  ok.

init_dbs() ->
  {ok, Stores} = application:get_env(barrel, stores),
  lists:foreach(fun({Store, _, _}) ->
      Dbs = barrel_store:all_dbs(Store),
      [barrel_db:start(Name, Store) || Name <- Dbs]
    end, Stores).
