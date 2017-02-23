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
  init_feed/3,
  wait_changes/1
]).

-export([
  system_continue/3,
  system_code_change/4,
  system_terminate/4
]).

-type listener_options() :: #{
  since => non_neg_integer(),
  mode => binary | sse,
  include_doc => true | false,
  history => true | false,
  change_cb => fun( (barrel_peer:change()) -> ok )
}.

-export_type([listener_options/0]).

-define(TIMEOUT, 5000).

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
  after ?TIMEOUT ->
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
-spec start_link(Conn, ListenerOptions) -> Res when
  Conn :: barrel_httpc:conn(),
  ListenerOptions :: listener_options(),
  ListenerPid :: pid(),
  Res :: {ok, ListenerPid} | {error, any()}.
start_link(Conn, Options) ->
  proc_lib:start_link(?MODULE, init_feed, [self(), Conn, Options]).

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



init_feed(Parent, Conn, Options) ->
  Headers = case maps:get(since, Options, 0) of
    0 -> 
      [{<<"Accept">>, <<"text/event-stream">>}];
    Since ->
      [{<<"Accept">>, <<"text/event-stream">>},
       {<<"Last-Event-Id">>, integer_to_binary(Since)}]
  end,
  Params = parse_options(Options),
  Url = barrel_httpc_lib:make_url(Conn, <<"docs">>, Params),
  ReqOpts = [{pool, none}, {async, once}],
  proc_lib:init_ack(Parent, {ok, self()}),
  case hackney:request(<<"GET">>, Url, Headers, <<>>, ReqOpts) of
    {ok, Ref} ->
      wait_response(Parent, Ref, Options);
    Error ->
      lager:error("~s: ~p~n", [?MODULE_STRING, Error]),
      exit(Error)
  end.

wait_response(Parent, Ref, Options) ->
  receive
    {hackney_response, Ref, {status, 200, _}} ->
      Cb = maps:get(changes_cb, Options, nil),
      Mode = maps:get(mode, Options, binary),
      State = #{parent => Parent,
                ref => Ref,
                changes => queue:new(),
                mode => Mode,
                changes_cb => Cb,
                buffer => <<>>},
      wait_changes(State);
    {hackney_response, Ref, {status, 404, _}} ->
      lager:error("~s not_found ~n", [?MODULE_STRING]),
      cleanup(Ref, not_found),
      exit(not_found);
    {hackney_response, Ref, {status, Status, Reason}} ->
      lager:error(
        "~s request bad status ~p(~p)~n",
        [?MODULE_STRING, Status, Reason]
      ),
      cleanup(Ref, {http_error, Status, Reason}),
      exit({http_error, Status, Reason});
    {hackney_response, Ref, {error, Reason}} ->
      lager:error(
        "~s hackney error: ~p~n",
        [?MODULE_STRING, Reason]
      ),
      cleanup(Ref, Reason),
      exit(Reason())
  after ?TIMEOUT ->
    cleanup(Ref, timeout),
    exit(timeout)
  end.

wait_changes(State = #{ parent := Parent, ref := Ref }) ->
  hackney:stream_next(Ref),
  receive
    {get_changes, Pid, Tag} ->
      {Events, NewState} = get_changes(State),
      Pid ! {changes, Tag, Events},
      wait_changes(NewState);
    {hackney_response, Ref, {headers, _Headers}} ->
      wait_changes(State);
    {hackney_response, Ref, done} ->
      cleanup(State, "remote stopped"),
      exit(normal);
    {hackney_response, Ref, Data} when is_binary(Data) ->
      decode_data(Data, State);
    {hackney_response, Ref, Error} ->
      lager:error(
        "~s hackney error: ~p~n",
        [?MODULE_STRING, Error]
      ),
      cleanup(State, Error),
      exit(Error);
    stop ->
      cleanup(State, "listener stopped"),
      exit(normal);
    {system, From, Request} ->
      sys:handle_system_msg(
        Request, From, Parent, ?MODULE, [],
        {wait_changes, State})
  after ?TIMEOUT ->
    lager:error("~s timeout: ~n", [?MODULE_STRING]),
    cleanup(State, timeout),
    exit(timeout)
  end.

system_continue(_, _, {wait_changes, State}) ->
  wait_changes(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, #{ ref := Ref }) ->
  %% unregister the stream
  catch hackney:close(Ref),
  lager:debug(
    "~s terminate: ~p",
    [?MODULE_STRING,Reason]
  ),
  exit(Reason).

system_code_change(Misc, _, _, _) ->
  {ok, Misc}.


cleanup(#{ ref := Ref }, Reason) ->
  cleanup(Ref, Reason);
cleanup(Ref, Reason) ->
  lager:info("closing change feed connection: ~p", [Reason]),
  (catch hackney:close(Ref)),
  
  ok.


get_changes(State = #{ changes := Q }) ->
  Changes = queue:to_list(Q),
  {Changes, State#{ changes => queue:new() }}.

decode_data(Data, State = #{ mode := binary, changes := Q, changes_cb := nil }) ->
  {Changes, NewState} = sse_changes(Data, State),
  Q2  = lists:foldl(
    fun
      (<<>>, Q1) -> Q1;
      (Change, Q1) -> queue:in(Change, Q1)
    end,
    Q,
    Changes
  ),
  wait_changes(NewState#{ changes => Q2 });
decode_data(Data, State = #{ changes := Q, changes_cb := nil }) ->
  {Changes, NewState} = sse_changes(Data, State),
  Q2  = lists:foldl(
    fun
      (<<>>, Q1) -> Q1;
      (Change, Q1) -> queue:in(parse_change(Change), Q1)
    end,
    Q,
    Changes
  ),
  wait_changes(NewState#{ changes => Q2 });
decode_data(Data, State = #{ mode := binary, changes_cb := Cb }) ->
  {Changes, NewState} = sse_changes(Data, State),
  lists:foreach(
    fun
      (<<>>) -> ok;
      (Change) -> Cb(Change)
    end,
    Changes
  ),
  wait_changes(NewState);
decode_data(Data, State = #{ changes_cb := Cb }) ->
  {Changes, NewState} = sse_changes(Data, State),
  lists:foreach(
    fun
      (<<>>) -> ok;
      (Change) -> Cb(parse_change(Change)) end,
    Changes
  ),
  wait_changes(NewState).

sse_changes(Data, State=#{ buffer := Buffer }) ->
  NewBuffer = << Buffer/binary, Data/binary >>,
  DataList = binary:split(NewBuffer, <<"\n\n">>, [global]),
  case lists:reverse(DataList) of
    [<<>> | Changes] ->
      {lists:reverse(Changes), State#{ buffer => <<>> }};
    [Rest | Changes] ->
      {lists:reverse(Changes), State#{ buffer => Rest }}
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
