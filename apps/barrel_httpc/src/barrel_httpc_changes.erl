%% Copyright 2017, Benoit Chesneau
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

-module(barrel_httpc_changes).

-export([
  start_link/2,
  parse_change/1,
  changes/1,
  stop/1
]).

-export([
  init/3,
  init_feed/1,
  wait_changes/1
]).

-export([
  system_continue/3,
  system_code_change/4,
  system_terminate/4
]).

-type listener_options() :: #{
  since              => non_neg_integer(),
  mode               => binary | sse,
  include_doc        => true | false,
  history            => true | false,
  changes_cb         => fun( (barrel_peer:change()) -> ok ),
  timeout            => non_neg_integer(),
  max_retry          => non_neg_integer(),
  delay_before_retry => non_neg_integer()
}.

-export_type([listener_options/0]).

-define(DEFAULT_TIMEOUT,   60000).
-define(DEFAULT_HEARTBEAT, 30000).
-define(DEFAULT_MAX_RETRY, 3).

-define(RETRY_TIMEOUT, 5000).

-record(state, {
          parent             :: pid(),
          conn,
          ref,
          hackney_timeout    :: non_neg_integer(),
          last_seq           :: undefined | non_neg_integer(),
          since              :: undefined | non_neg_integer(),
          changes,
          mode               :: term(),
          changes_cb,
          buffer,
          retry              :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
          options            :: map()
         }).
%% fetch all changes received by a listener à that time.
%% Only useful when no changes callback is given.
%% Otherwise the list will always be empty.
-spec changes(ListenerPid) -> Changes when
  ListenerPid :: pid(),
  Changes :: [barrel_httpc:change()].
changes(FeedPid) ->
  Tag = make_ref(),
  MRef = erlang:monitor(process, FeedPid),
  FeedPid ! {get_changes, self(), Tag},
  receive
    {changes, Tag, Changes} -> Changes;
    {'DOWN', MRef, _, _, Reason} -> exit(Reason)
  after ?DEFAULT_TIMEOUT ->
    erlang:demonitor(MRef, [flush]),
    exit(timeout)
  end.

%% @doc start a change listener on the database.
%% This function create a process that will listen on the changes feed API.
%% If not callback is given, changes are queued in the process and need
%% to be fetched using the `fetch_changes' function. When a callback is given,
%% a change is passed to the function, no state is kept in the process.
%% a change given to the callback or in the list is under the following form
%% #{
%%   <<"id">> := binary(),  % id of the document updated
%%   <<"seq">> := non_neg_integer(), % sequence of the change
%%   <<"changes">> => [revid(], % revision id of the change or
%%                              % the full history if history is true (from last to first),
%%   <<"deleted">> => true | false % present if deleted
%%}
%%
%% In case the connection is lost or closed, it will retry to connect, at most
%% `max_retry` times (default=5 times), waiting `delay_before_retry` ms between each
%% try (default=500 ms)
-spec start_link(Conn, ListenerOptions) -> Res when
  Conn :: barrel_httpc:conn(),
  ListenerOptions :: listener_options(),
  ListenerPid :: pid(),
  Res :: {ok, ListenerPid} | {error, any()}.
start_link(Conn, Options) when is_map(Options) ->
  proc_lib:start_link(?MODULE, init, [self(), Conn, Options]).

%% @doc stop a change listener
-spec stop(ListenerPid) -> Res when
  ListenerPid :: pid(),
  Res :: ok.
stop(FeedPid) ->
  MRef = erlang:monitor(process, FeedPid),
  FeedPid ! stop,
  receive
    {'DOWN', MRef, _, _, _} -> ok
  end.

%% @doc parse a binary change fetched when start_listener mod is binary
-spec parse_change(binary()) -> barrel_httpc:change().
parse_change(ChangeBin) ->
  Lines = binary:split(ChangeBin, <<"\n">>, [global]),
  lists:foldl(
    fun(Line, Acc) ->
      case Line of
        << "data: ", Change/binary >> ->
          jsx:decode(Change, [return_maps]);
        _ ->
          Acc
      end
    end,
    #{},
    Lines
  ).

init(Parent, Conn, Options) ->
  process_flag(trap_exit, true),
  proc_lib:init_ack(Parent, {ok, self()}),
  %% initialize the state
  State = #state{ parent = Parent,
                  conn = Conn,
                  retry = reset_retry(Options),
                  hackney_timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
                  options = Options},
  init_feed(State).

reset_retry(Options) ->
  Retries = maps:get(retry, Options, ?DEFAULT_MAX_RETRY),
  RetryTimeout =  maps:get(retry_timeout, Options, ?RETRY_TIMEOUT),
  {Retries, 200, RetryTimeout}.


init_feed(State) ->
  #state{
    conn = Conn,
    last_seq = LastSeq,
    options = Options
  } = State,

  Since = case LastSeq of
            undefined -> maps:get(since, Options, 0);
            Seq -> Seq
          end,
  Headers = case Since of
    0 ->
      [{<<"Accept">>, <<"text/event-stream">>}];
    Since ->
      [{<<"Accept">>, <<"text/event-stream">>},
       {<<"Last-Event-Id">>, integer_to_binary(Since)}]
  end,
  Params0 = parse_options(Options),
  Params = case proplists:is_defined(<<"heartbeat">>, Params0) of
             true -> Params0;
             _ -> [{<<"heartbeat">>, ?DEFAULT_HEARTBEAT}|Params0]
           end,
  Url = barrel_httpc_lib:make_url(Conn, <<"docs">>, Params),
  ReqOpts = [{pool, none}, {async, once}, {recv_timeout, infinity}],
  case hackney:request(<<"GET">>, Url, Headers, <<>>, ReqOpts) of
    {ok, Ref} ->
      wait_response(State#state{ ref = Ref, retry = reset_retry(Options) });
    Error ->
      _ = lager:error("~s: ~p~n", [?MODULE_STRING, Error]),
      maybe_retry(State, Error)
  end.

maybe_retry(State, ExitReason) ->
  #state{ ref = Ref, retry = {Retries, Delay, Max} } = State,
  _ = (catch hackney:close(Ref)),
  if
    Retries /= 0 ->
      _ = lager:warning("~s retrying connection...~n", [?MODULE_STRING]),
      _ = erlang:send_after(Delay, self(), connect),
      State2 = State#state{ retry={Retries - 1, rand_increment(Delay, Max), Max} },
      wait_retry(State2);
    true ->
      _ = lager:warning("~s: num of retries exceeded the limit.~n", [?MODULE_STRING]),
      cleanup(State, ExitReason),
      exit(normal)
  end.

wait_retry(State = #state{parent = Parent}) ->
  receive
    connect ->
      init_feed(State);
    {'EXIT', Parent, _} ->
      cleanup(State, "parent stopped"),
      exit(normal);
    {system, From, Request} ->
      sys:handle_system_msg(
        Request, From, Parent, ?MODULE, [],
        {wait_retry, State})
  end.

wait_response(#state{ parent = Parent, ref = Ref, options = Options}=State) ->
  receive
    {hackney_response, Ref, {status, 200, _}} ->
      Conn = State#state.conn,
      _ = lager:info("[~s] connected to conn=~p", [?MODULE_STRING, Conn]),
      Cb = maps:get(changes_cb, Options, nil),
      Mode = maps:get(mode, Options, binary),
      State2 = State#state{changes = queue:new(), mode = Mode, changes_cb = Cb, buffer = <<>>},
      wait_changes(State2);
    {hackney_response, Ref, {status, 404, _}} ->
      _ = lager:error("~s not_found ~n", [?MODULE_STRING]),
      cleanup(Ref, not_found),
      exit(not_found);
    {hackney_response, Ref, {status, Status, Reason}} ->
      _ = lager:error(
        "~s request bad status ~p(~p)~n",
        [?MODULE_STRING, Status, Reason]
      ),
      maybe_retry(State, {http_error, Status, Reason});
    {hackney_response, Ref, {error, closed}} ->
      _ = lager:warning("[~s] hackney connection closed", [?MODULE_STRING]),
      maybe_retry(State, normal);
    {hackney_response, Ref, {error, Reason}} ->
      _ = lager:error(
        "~s hackney error: ~p~n",
        [?MODULE_STRING, Reason]
       ),
      cleanup(Ref, Reason),
      exit(Reason);
    {'EXIT', Parent, _} ->
      cleanup(State, "parent stopped"),
      exit(normal);
    {system, From, Request} ->
      sys:handle_system_msg(
        Request, From, Parent, ?MODULE, [],
        {wait_response, State})
  after State#state.hackney_timeout ->
    cleanup(State, timeout),
    exit(timeout)
  end.

wait_changes(#state{ parent = Parent, ref = Ref }=State) ->
  _ = hackney:stream_next(Ref),
  receive
    {get_changes, Pid, Tag} ->
      {Events, NewState} = get_changes(State),
      Pid ! {changes, Tag, Events},
      wait_changes(NewState);
    {hackney_response, Ref, {headers, _Headers}} ->
      wait_changes(State);
    {hackney_response, Ref, done} ->
      _ = lager:warning("[~s] hackney connection done", [?MODULE_STRING]),
      maybe_retry(State, normal);
    {hackney_response, Ref, Data} when is_binary(Data) ->
      decode_data(Data, State);
    {hackney_response, Ref, Error} ->
      _ = lager:error(
        "~s hackney error: ~p~n",
        [?MODULE_STRING, Error]
      ),
      cleanup(State, Error),
      exit(Error);
    stop ->
      cleanup(State, "listener stopped"),
      exit(normal);
    {'EXIT', Parent, _} ->
      cleanup(State, "parent stopped"),
      exit(normal);
    {system, From, Request} ->
      sys:handle_system_msg(
        Request, From, Parent, ?MODULE, [],
        {wait_changes, State})
  after State#state.hackney_timeout ->
    _ = lager:error("~s timeout: ~n", [?MODULE_STRING]),
    cleanup(State, timeout),
    exit(timeout)
  end.


system_continue(_, _, {wait_retry, State}) ->
  wait_retry(State);
system_continue(_, _, {wait_response, State}) ->
  wait_response(State);
system_continue(_, _, {wait_changes, State}) ->
  wait_changes(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, #{ ref := Ref }) ->
  %% unregister the stream
  catch hackney:close(Ref),
  _ = lager:debug(
    "~s terminate: ~p",
    [?MODULE_STRING,Reason]
  ),
  exit(Reason).

system_code_change(Misc, _, _, _) ->
  {ok, Misc}.


cleanup(#state{ ref = Ref }, Reason) ->
  cleanup(Ref, Reason);
cleanup(Ref, Reason) ->
  _ = lager:info("closing change feed connection: ~p", [Reason]),
  (catch hackney:close(Ref)),
  ok.


get_changes(#state{ changes = Q }=State) ->
  Changes = queue:to_list(Q),
  {Changes, State#state{ changes = queue:new() }}.

decode_data(Data, #state{ mode = binary, last_seq = OldSeq, changes = Q, changes_cb = nil }=State) ->
  {Changes, NewState} = sse_changes(Data, State),
  DecodeFun = fun(C) -> C end,
  {LastSeq, Q2} = queue_change(Changes, DecodeFun, OldSeq, Q),
  wait_changes(NewState#state{ last_seq = LastSeq, changes = Q2 });
decode_data(Data, #state{ changes = Q, last_seq = OldSeq, changes_cb = nil }=State) ->
  {Changes, NewState} = sse_changes(Data, State),
  {LastSeq, Q2} = queue_change(Changes, fun parse_change/1, OldSeq, Q),
  wait_changes(NewState#state{ last_seq = LastSeq, changes = Q2 });
decode_data(Data, #state{ mode = Mode, last_seq = OldSeq, changes_cb = Cb }=State) ->
  {Changes, NewState} = sse_changes(Data, State),
  LastSeq = handle_changes(Changes, Mode, Cb, OldSeq),
  wait_changes(NewState#state{last_seq = LastSeq}).


queue_change([<<>> | Rest], Fun, Last, Queue) ->
  queue_change(Rest, Fun, Last, Queue);
queue_change([Change | Rest], Fun, _Last, Queue) ->
  #{<<"seq">> := Seq} = parse_change(Change),
  queue_change(Rest, Fun, Seq, queue:in(Fun(Change), Queue));
queue_change([], _Fun, Last, Queue) ->
  {Last, Queue}.

handle_changes([<<>> | Rest], Mode, Cb, Last) ->
  handle_changes(Rest, Mode, Cb, Last);
handle_changes([ChangeBin | Rest], Mode, Cb, _Last) ->
  #{<<"seq">> := Seq} = Change = parse_change(ChangeBin),
  case Mode of
    binary -> Cb(ChangeBin);
    _ -> Cb(Change)
  end,
  handle_changes(Rest, Mode, Cb, Seq);
handle_changes([], _Mode, _Cb, Last) ->
  Last.


sse_changes(Data, #state{ buffer = Buffer }=State) ->
  NewBuffer = << Buffer/binary, Data/binary >>,
  DataList = binary:split(NewBuffer, <<"\n\n">>, [global]),
  case lists:reverse(DataList) of
    [<<>> | Changes] ->
      {lists:reverse(Changes), State#state{ buffer = <<>> }};
    [Rest | Changes] ->
      {lists:reverse(Changes), State#state{ buffer = Rest }}
  end.

parse_options(Options) ->
  maps:fold(
    fun
      (include_doc, IncludeDocs, Acc) ->
        [{<<"include_doc">>, IncludeDocs} | Acc];
      (history, History, Acc) ->
        [{<< "history" >>, History} | Acc];
      (heartbeat, Heartbeat, Acc) ->
        [{<< "heartbeat" >>, Heartbeat} | Acc];
      (_, _, Acc) -> Acc
    end,
    [],
    Options
  ).

rand_increment(N) ->
  %% New delay chosen from [N, 3N], i.e. [0.5 * 2N, 1.5 * 2N]
  Width = N bsl 1,
  N + rand:uniform(Width + 1) - 1.

rand_increment(N, Max) ->
  %% The largest interval for [0.5 * Time, 1.5 * Time] with maximum Max is
  %% [Max div 3, Max].
  MaxMinDelay = Max div 3,
  if
    MaxMinDelay =:= 0 ->
      rand:uniform(Max);
    N > MaxMinDelay ->
      rand_increment(MaxMinDelay);
    true ->
      rand_increment(N)
  end.
