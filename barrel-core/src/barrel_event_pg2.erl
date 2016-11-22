-module(barrel_event_pg2).
-author("Benoit Chesneau").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  broadcast/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


%% API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

broadcast(Topic, Event) ->
  case pg2:get_members(barrel_events_service) of
    {error, {no_such_group, _}} ->
      {error, no_such_group};
    Pids when is_list(Pids) ->
      lists:foreach(fun
                      (Pid) when node(Pid) =:= node() ->
                        barrel_event_local:broadcast(Topic, Event);
                      (Pid) ->
                        Pid ! {forward_to_local, Topic, Event}
                    end, Pids)
  end.

%% gen_server callbaks

init([]) ->
  pg2:create(barrel_events_service),
  pg2:join(barrel_events_service, self()),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({forward_to_local, Topic, Event}, State) ->
  _ = barrel_event_local:broadcast(Topic, Event),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

