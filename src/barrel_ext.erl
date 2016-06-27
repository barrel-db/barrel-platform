%% Copyright (c) 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.
%%

%% Created by benoitc on 27/06/16.

-module(barrel_ext).
-author("Benoit Chesneau").

-export([start/0, stop/0, start_extensions/0, stop_extensions/0, start_extension/2, stop_extension/2]).

-include("log.hrl").

-type opts() :: [{atom(), any()}].

%%====================================================================
%% Callbacks definition
%%====================================================================

-callback start(opts()) -> any().
-callback stop() -> any().

%%====================================================================
%% API
%%====================================================================

start() ->
  ets:new(barrel_ext, [named_table, public]),
  start_extensions().

stop() ->
  stop_extensions().

start_extensions() ->
  Extensions = barrel_config:get_env(extensions),
  start_extensions(Extensions).

start_extensions([{M, O} | Rest]) when is_atom(M), is_list(O) ->
  _ = start_extension(M, O),
  start_extensions(Rest);
start_extensions([]) ->
  ok.

stop_extensions() ->
  Extensions = ets:tab2list(barrel_ext),
  stop_extensions(Extensions).

stop_extensions([{M, O} | Rest]) ->
  _ = stop_extension(M, O),
  stop_extensions(Rest);
stop_extensions([]) ->
  ok.

start_extension(Mod, Opts) ->
  _ = code:ensure_loaded(Mod),
  case erlang:function_exported(Mod, start, 1) of
    true ->
      ets:insert(barrel_ext, {Mod, Opts}),
      try Mod:start(Opts)
      catch
        Class:Reason ->
          ets:delete(barrel_ext, Mod),
          ?log(error, "Problem starting ~s with ~p~n: ~p:~p~n", [Mod, Opts, Class, Reason]),
          erlang:raise(Class, Reason, erlang:get_stacktrace())
      end;
    false ->
      ?log(info, "~s not started. start function is missing~n", [Mod])
  end.

stop_extension(Mod, Opts) ->
  case catch Mod:stop(Opts) of
    {'EXIT', Reason} -> ?log(error, "error when stopping ~s:~n ~p", [Mod, Reason]);
    {wait, Pids} when is_list(Pids) -> wait_pids(Pids);
    {wait, Pid} when is_pid(Pid) -> wait_pid(Pid);
    _ -> ok
  end,
  ets:delete(barrel_ext, Mod).

wait_pids(Pids) -> [wait_pid(Pid) || Pid <- Pids].

wait_pid(Pid) ->
  MRef = erlang:monitor(process, Pid),
  receive
    {'DOWN', MRef, _, _, _} -> ok
  after 5000 ->
    catch exit(Pid, kill),
    receive
      {'DOWN', MRef, _, _, _} -> ok
      after 5000 ->
        ok
    end
  end.
