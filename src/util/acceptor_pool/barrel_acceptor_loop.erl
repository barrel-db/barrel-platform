%%-------------------------------------------------------------------
%%
%% Copyright (c) 2016, James Fish <james@fishcakez.com>
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%-------------------------------------------------------------------
%% @private
-module(barrel_acceptor_loop).

-export([accept/5]).
-export([await/5]).

-callback acceptor_continue({ok, Sock} | {error, Reason}, Parent, Misc) ->
  no_return() when
  Sock :: gen_tcp:socket(),
  Reason :: timeout | closed | system_limit | inet:posix(),
  Parent :: pid(),
  Misc :: term().

-callback acceptor_terminate(Reason, Parent, Misc) -> no_return() when
  Reason :: term(),
  Parent :: pid(),
  Misc :: term().

-spec accept(LSock, TimeoutOrHib, Parent, Mod, Misc) -> no_return() when
  LSock :: gen_tcp:socket(),
  TimeoutOrHib :: timeout() | hibernate,
  Parent :: pid(),
  Mod :: module(),
  Misc :: term().
accept(LSock, hibernate, Parent, Mod, Misc) ->
  case prim_inet:async_accept(LSock, -1) of
    {ok, InetRef} ->
      Args = [LSock, InetRef, Parent, Mod, Misc],
      proc_lib:hibernate(?MODULE, await, Args);
    {error, _} = Error ->
      Mod:acceptor_continue(Error, Parent, Misc)
  end;
accept(LSock, Timeout, Parent, Mod, Misc) ->
  accept_timeout(LSock, Timeout, Parent, Mod, Misc).

-spec await(LSock, InetRef, Parent, Mod, Misc) -> no_return() when
  LSock :: gen_tcp:socket(),
  InetRef :: term(),
  Parent :: pid(),
  Mod :: module(),
  Misc :: term().
await(LSock, InetRef, Parent, Mod, Misc) ->
  receive
    {inet_async, LSock, InetRef, Result} ->
      Mod:acceptor_continue(Result, Parent, Misc);
    {'EXIT', Parent, Reason} ->
      Mod:acceptor_terminate(Reason, Parent, Misc)
  end.

%% internal

accept_timeout(LSock, infinity, Parent, Mod, Misc) ->
  accept_timeout(LSock, -1, Parent, Mod, Misc);
accept_timeout(LSock, Timeout, Parent, Mod, Misc) ->
  case prim_inet:async_accept(LSock, Timeout) of
    {ok, InetRef}      -> await(LSock, InetRef, Parent, Mod, Misc);
    {error, _} = Error -> Mod:acceptor_continue(Error, Parent, Misc)
  end.