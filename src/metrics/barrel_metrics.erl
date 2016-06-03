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

-module(barrel_metrics).
-behaviour(gen_server).

-export([reload/0]).
-export([list/0]).

-export([start_link/0]).

%% gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
  terminate/2]).

-define(DEFAULT_INTERVAL, 5000).  % change with option interval
-define(MIN_INTERVAL, 100).

-record(state, {interval,
  timer,
  by_apps,
  specs}).

% ----------------------------------------------------------
% - public api
% ----------------------------------------------------------

reload() ->
  gen_server:cast(?MODULE, reload).

list() ->
  gen_server:call(?MODULE, list_metrics).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% ----------------------------------------------------------
% - gen_server api
% ----------------------------------------------------------

init(_) ->
  self() ! init,
  {ok, #state{interval=interval()}}.

handle_call(list_metrics, _From, State) ->
  {reply, State#state.specs, State};

handle_call(_Msg, _From, State) ->
  {reply, bad_call, State}.

handle_cast(reload, State) ->
  {noreply, do_reload(State)};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(init, State) ->
  {noreply, do_init(State)};

handle_info(watch, State) ->
  {noreply, do_watch(State)};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  _ = timer:cancel(State#state.timer), %% make sure to close the timer
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


% ----------------------------------------------------------
% - private api
% ----------------------------------------------------------

do_init(State) ->
  {ByApps, Specs} = load_metrics(),
  init_metrics(Specs),
  {ok, Timer} = timer:send_interval(State#state.interval, self(), watch),
  State#state{timer=Timer, by_apps=ByApps, specs=Specs}.

do_reload(State) ->
  delete_metrics(State#state.specs),
  {ByApps, Specs} = load_metrics(),
  init_metrics(Specs),
  State#state{by_apps=ByApps, specs=Specs}.


do_watch(State) ->
  #state{by_apps=OldByApps, specs=OldSpecs} = State,
  %% unloaded and new loaded applications in the vm
  Loaded = list_applications(),
  Watched = maps:keys(OldByApps),
  Unloaded = Watched -- Loaded,
  ToWatch = Loaded -- Watched,

  %% reload configuration if needed
  {ByApps0, Specs0} = maybe_reload_metrics(maps:without(Unloaded, OldByApps)),

  %% load new metrics
  {NewApps, NewSpecs} = load_metrics(ToWatch),

  %% merge results
  ByApps= maps:merge(ByApps0, NewApps),
  Specs = lists:usort(Specs0 ++ NewSpecs),

  %% clean metrics
  delete_metrics(OldSpecs -- Specs),
  init_metrics(Specs -- OldSpecs),

  State#state{by_apps=ByApps, specs=Specs}.


%% @private

interval() ->
  N = application:get_env(barrel_metrics, inteval, ?DEFAULT_INTERVAL),
  safe_interval(N).

safe_interval(N) when is_integer(N) ->
  erlang:min(16#FFFFffff, erlang:max(N, ?MIN_INTERVAL));
safe_interval(_) -> ?DEFAULT_INTERVAL.

init_metrics(Specs) ->
  lists:foreach(fun({Section, Name, Type, _Desc}) ->
    _ = exometer:new([Section, Name], Type)
                end, Specs),
  ok.

delete_metrics([]) -> ok;
delete_metrics(OldSpecs) ->
  lists:foreach(fun({Section, Name, _Type, _Desc}) ->
    _ = exometer:delete([Section, Name])
                end, OldSpecs).


list_applications() ->
  [element(1, A) || A <- application:loaded_applications()].


maybe_reload_metrics(Apps) ->
  maps:fold(fun(App, {AppSpecs, Mod}, {Apps1, Specs1}) ->
    case code:priv_dir(App) of
      {error, _Reason} ->
        {Apps1, Specs1};
      Dir ->
        Path = filename:join(Dir, "stats_descriptions.cfg"),
        case filelib:last_modified(Path) of
          0 ->
            {Apps1, Specs1};
          LastMod when LastMod > Mod ->
            case file:consult(Path) of
              {ok, Specs} ->
                {Apps#{App => {Specs, LastMod}}, Specs1 ++ Specs};
              error ->
                lager:error("error reloading metrics conf for ~p~n", [App]),
                {Apps1#{ App => {AppSpecs, LastMod}}, Specs1 ++ AppSpecs}
            end;
          _ ->
            {Apps1#{ App => {AppSpecs, Mod}}, Specs1 ++ AppSpecs}
        end
    end
            end, {#{}, []}, Apps).


load_metrics() ->
  load_metrics(list_applications()).

load_metrics(Apps) ->
  {ByApps, Specs} = lists:foldl(fun(App, {ByApps1, Specs1}=Acc) ->
    case load_app_metrics(App) of
      {ok, Specs2, LastMod} ->
        {ByApps1#{ App => {Specs2, LastMod} }, Specs2 ++ Specs1};
      error ->
        Acc
    end
                                end, {#{}, []}, Apps),
  {ByApps, lists:usort(Specs)}.

load_app_metrics(AppName) ->
  case code:priv_dir(AppName) of
    {error, _Error} -> error;
    Dir ->
      Path = filename:join(Dir, "stats_descriptions.cfg"),
      case file:consult(Path) of
        {ok, Specs} ->
          case validate_specs(Specs) of
            ok ->
              LastMod = filelib:last_modified(Path),
              {ok, Specs, LastMod};
            bad_spec ->
              lager:error("~p: stats_descriptions.cfg is invalid.~n", [AppName]),
              error
          end;
        {error, _Error} ->
          error
      end
  end.

validate_specs([]) ->
  ok;
validate_specs([{_Section, _Name, Type, ShortDesc} | Rest])
  when is_atom(Type), is_list(ShortDesc) ->
  validate_specs(Rest);
validate_specs([_|_Rest]) ->
  bad_spec.