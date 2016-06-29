
%% Copyright (c) 2016. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Created by benoitc on 19/06/16.

-module(barrel).
-author("Benoit Chesneau").

%% API

-export([
  databases/0,
  fold_databases/2
]).


-export([
  start_listener/2,
  stop_listener/1,
  start_console/1,
  stop_console/0,
  info/0
]).

-type dbname() :: string() | binary().

%%%===================================================================
%%% Database API
%%%===================================================================

%% @doc get the list of all databases
-spec databases() -> [dbname()].
databases() -> barrel_server:all_databases().

%% @doc fold all databases name
-spec fold_databases(fun(), any()) -> any().
fold_databases(Fun, Acc) -> barrel_server:all_databases(Fun, Acc).


%%%===================================================================
%%% Server API
%%%===================================================================


-spec start_listener(atom(), list()) -> ok | {error, term()}.
start_listener(Ref, Opts) when is_atom(Ref) ->
  Res = supervisor:start_child(barrel_api_sup, barrel_api_http:binding_spec(Ref, Opts)),
  case Res of
    {ok, _Pid} ->
      Listeners = [{Ref, Opts} | barrel_lib:val(listen, [])],
      barrel_lib:set(listeners, Listeners),
      ok;
    Error ->
      Error
  end.

stop_listener(Ref) ->
  case supervisor:terminate_child(barrel_api_sup, {ranch_listener_sup, Ref}) of
    ok ->
      _ = supervisor:delete_child(barrel_api_sup, {ranch_listener_sup, Ref}),
      barrel_api_http:cleanup_listener_opts(Ref);
    {error, Reason} ->
      {error, Reason}
  end.

start_console(Opts) ->
  Res = supervisor:start_child(barrel_api_sup, barrel_http_console:childspec(Opts)),
  case Res of
    {ok, _Pid} ->
      barrel_lib:set(start_console, true),
      barrel_lib:set(console, Opts),
      ok;
    Error ->
      Error
  end.

stop_console() ->
  case supervisor:terminate_child(barrel_api_sup, {ranch_listener_sup, barrel_console}) of
    ok ->
      _ = supervisor:delete_child(barrel_api_sup, {ranch_listener_sup, barrel_console}),
      barrel_lib:unset(console),
      barrel_lib:set(start_console, false),
      ranch_server:cleanup_listener_opts(barrel_console);
    Error ->
      Error
  end.

info() -> barrel_server:node_info().
