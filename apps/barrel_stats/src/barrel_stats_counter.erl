-module(barrel_stats_counter).
-behaviour(gen_server).

%% public api
-export([
  create/2,
  record/2,
  set/3,
  value/2,
  values/1,
  reset/1,
  reset/2,
  reset_all/0
]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,  code_change/3]).


-include_lib("stdlib/include/ms_transform.hrl").

%%%===================================================================
%%% API
%%%===================================================================


create(Name, Labels) ->
  gen_server:call(?MODULE, {create, Name, Labels}).

record(Name, Labels) ->
  Key = {Name, Labels},
  case erlang:get({barrel_counter_ref, Key}) of
    undefined ->
      Ref =
        try ets:lookup_element(?MODULE, Key, 2) of
          R -> R
        catch
          _:_ ->
            ok = create(Name, Labels),
            ets:lookup_element(?MODULE, Key, 2)
        end,
      erlang:put({barrel_counter_ref, Name}, Ref),
      mzmetrics:incr_resource_counter(Ref, 0);
    Ref ->
      mzmetrics:incr_resource_counter(Ref, 0)
  end,
  ok.


set(Name, Labels, Value) ->
  Key = {Name, Labels},
  case erlang:get({barrel_counter_ref, Key}) of
    undefined ->
      Ref =
        try ets:lookup_element(?MODULE, Key, 2) of
          R -> R
        catch
          _:_ ->
            ok = create(Name, Labels),
            ets:lookup_element(?MODULE, Key, 2)
        end,
      erlang:put({barrel_counter_ref, Name}, Ref),
      mzmetrics:update_resource_counter(Ref, 0, Value);
    Ref ->
      mzmetrics:update_resource_counter(Ref, 0, Value)
  end,
  ok.

value(Name, Labels) ->
  Key = {Name, Labels},
  Ref =
    case erlang:get({barrel_counter_ref, Key}) of
      undefined ->
        case ets:lookup(?MODULE, Key) of
          [{_, R}] ->
            erlang:put({barrel_counter_ref, Key}, R),
            R;
          [] -> erlang:error(not_found)
        end;
      R -> R
    end,
  mzmetrics:get_resource_counter(Ref, 0).

values(Name) ->
  Spec = ets:fun2ms(fun({{N, _}, _}=Counter) when N =:= Name -> Counter end),
  Counters = ets:select(?MODULE, Spec),
  [{{N, L}, mzmetrics:get_resource_counter(Ref, 0)} || {{N, L}, Ref} <- Counters].


reset(Name) ->
  Spec = ets:fun2ms(fun({{N, _}, Ref}) when N =:= Name -> Ref end),
  Refs = ets:select(?MODULE, Spec),
  _ = [mzmetrics:reset_resource_counter(Ref, 0) || Ref <- Refs],
  ok.

reset(Name, Labels) ->
  Key = {Name, Labels},
  case erlang:get({barrel_counter_ref, Key}) of
    undefined ->
      Ref = ets:lookup_element(?MODULE, Key, 2),
      erlang:put({barrel_counter_ref, Key}, Ref),
      mzmetrics:reset_resource_counter(Ref, 0);
    Ref ->
      mzmetrics:reset_resource_counter(Ref, 0)
  end.

reset_all() ->
  Counters = ets:tab2list(?MODULE),
  _ = [mzmetrics:reset_resource_counter(Ref, 0) ||{_, Ref} <- Counters],
  ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  _ = ets:new(?MODULE,  [set, public, named_table, {read_concurrency, true}]),
  {ok, []}.

handle_call({create, Name, Labels}, _From, State) ->
  Ref =  mzmetrics:alloc_resource(0, barrel_stats_lib:uniqid(), 8),
  _ = ets:insert_new(?MODULE, {{Name, Labels}, Ref}),
  {reply, ok, State};

handle_call(Req, _From, State) ->
  _ = lager:error("Unhandled call: ~p", [Req]),
  {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
  _ = lager:error("Unhandled cast: ~p", [Msg]),
  {stop, {unhandled_cast, Msg}, State}.

handle_info(Info, State) ->
  _ = lager:error("Unhandled info: ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
