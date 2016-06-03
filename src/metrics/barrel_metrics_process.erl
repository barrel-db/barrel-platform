%% Copyright 2016 Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://joinup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

%% @doc module to track processes associated to a counter
-module(barrel_metrics_process).
-behaviour(gen_server).

%% public API
-export([track/1, track/2]).

%% internal API
-export([start_link/0]).

%% gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
  terminate/2]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(TAB, barrel_metrics).
-record(state, {}).

%% @doc track the number of processes for a name
%% this add the current process
-spec track(any()) -> ok | {error, term()}.
track(Name) ->
  track(Name, self()).

%% @doc track the number of processes for a name
-spec track(any(), pid()) -> ok | {error, term()}.
track(Name, Pid) ->
  %% we only increment once the value
  case ets:insert_new(?TAB, {{Pid, Name}, Name}) of
    true -> exometer:update(Name, 1);
    false -> ok
  end,

  %% maybe monitor the pid
  case ets:insert_new(?TAB, {Pid, m}) of
    true -> gen_server:cast(?MODULE, {monitor, Pid});
    false -> ok
  end.

% ----------------------------------------------------------
% - internal api
% ----------------------------------------------------------
start_link() ->
  _ = create_tabs(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


create_tabs() ->
  case ets:info(?TAB, name) of
    undefined ->
      ?TAB = ets:new(?TAB, [named_table, ordered_set, public,
        {write_concurrency, true},
        {read_concurrency, true}]);
    _ ->
      true
  end.

% ----------------------------------------------------------
% - gen_server api
% ----------------------------------------------------------
init(_) ->
  init_monitors(),
  {ok, #state{}}.

handle_call(_Msg, _From, State) ->
  {reply, bad_call, State}.

handle_cast({monitor, Pid}, State) ->
  _ = erlang:monitor(process, Pid),
  {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
  _ = process_is_down(Pid),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


% ----------------------------------------------------------
% - private api
% ----------------------------------------------------------

%% @private
process_is_down(Pid) ->
  case ets:take(?TAB, Pid) of
    [] ->
      ok;
    [{Pid, m}] ->
      Cond = ets:fun2ms(fun({{A, B}, _}) when A =:= Pid -> B end),
      Names = ets:select(?TAB, Cond),
      case Names of
        [] -> ok;
        _ ->
          lists:foreach(fun(Name) ->
            exometer:update(Name, -1),
            ets:delete(?TAB, {Pid, Name})
                        end, Names)
      end
  end.

init_monitors() ->
  Cond = ets:fun2ms(fun({A, B}) when B =:= m -> A end),
  Pids = ets:select(?TAB, Cond),
  [erlang:monitor(process, Pid) || Pid <- Pids].
