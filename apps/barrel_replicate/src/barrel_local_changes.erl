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

-module(barrel_local_changes).
-author("benoitc").

%% API
-export([
  start_link/2,
  stop/1,
  changes/1,
  parse_change/1
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

-include_lib("barrel_store/include/barrel_store.hrl").

-define(TIMEOUT, 5000).

start_link(DbId, Options) ->
  case barrel_store:whereis_db(DbId) of
    undefined ->
      lager:debug(
        "~s: db ~p not found",
        [?MODULE_STRING, DbId]
      ),
      {error, db_not_found};
    Db ->
      proc_lib:start_link(?MODULE, init_feed, [self(), Db, Options])
  end.


stop(FeedPid) ->
  MRef = erlang:monitor(process, FeedPid),
  FeedPid ! stop,
  receive
    {'DOWN', MRef, _, _, _} -> ok
  end.

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


init_feed(Parent, Db, Options) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  Since = maps:get(since, Options, 0),
  ChangeCb = maps:get(changes_cb, Options, nil),
  Mode = maps:get(mode, Options, binary),
  
  %% make change callback
  ChangeFun = case {Mode, ChangeCb} of
                {binary, nil} ->
                  fun(Change, {LastSeq, ChangeQ}) ->
                    Seq = maps:get(<<"seq">>, Change),
                    Encoded = encode_sse(Change),
                    {ok, {erlang:max(LastSeq, Seq), queue:in(Encoded, ChangeQ)}}
                  end;
                {binary, ChangeCb} ->
                  fun(Change, {LastSeq, ChangeQ}) ->
                    Seq = maps:get(<<"seq">>, Change),
                    ChangeCb(encode_sse(Change)),
                    {ok, {erlang:max(LastSeq, Seq), ChangeQ}}
                  end;
                {_, nil} ->
                  fun(Change, {LastSeq, ChangeQ}) ->
                    Seq = maps:get(<<"seq">>, Change),
                    {ok, {erlang:max(LastSeq, Seq), queue:in(Change, ChangeQ)}}
                  end;
                {_, ChangeCb} ->
                  fun(Change, {LastSeq, ChangeQ}) ->
                    Seq = maps:get(<<"seq">>, Change),
                    ChangeCb(Change),
                    {ok, {erlang:max(LastSeq, Seq), ChangeQ}}
                  end
              end,
  
  %% retrieve change options from the options given to the feed
  %% TODO: have changes_since function accepting a map
  ChangeOpts = parse_options(Options),
  
  Ref = erlang:monitor(process, Db#db.pid),
  
  %% initialize the changes
  State0 =
  #{parent => Parent,
    db => Db,
    db_ref => Ref,
    opts => ChangeOpts,
    change_fun => ChangeFun,
    changes => queue:new(),
    last_seq => Since},
  State1 = get_changes(State0),
  
  barrel_event:reg(Db#db.id),
  
  wait_changes(State1).


wait_changes(State = #{ parent := Parent , db_ref := Ref}) ->
  receive
    {get_changes, Pid, Tag} ->
      Changes = maps:get(changes, State),
      Pid ! {changes, Tag, queue:to_list(Changes)},
      wait_changes(State#{ changes => queue:new() });
    stop ->
      exit(normal);
    {'$barrel_event', _, db_updated} ->
      NewState = get_changes(State),
      wait_changes(NewState);
    {system, From, Request} ->
      sys:handle_system_msg(
        Request, From, Parent, ?MODULE, [],
        {wait_changes, State});
    {'DOWN', Ref, process, _Pid, _Reason} ->
      exit(normal)
  end.

system_continue(_, _, {wait_changes, State}) ->
  wait_changes(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _State) ->
  lager:debug(
    "~s terminate: ~p",
    [?MODULE_STRING,Reason]
  ),
  
  catch barrel_event:unreg(),
  exit(Reason).

system_code_change(Misc, _, _, _) ->
  {ok, Misc}.

get_changes(State) ->
  #{db := Db, opts :=  Opts, change_fun := ChangeFun, last_seq := LastSeq, changes := Changes} = State,
  {LastSeq1, Changes1} = barrel_db:changes_since(
    Db#db.id, LastSeq, ChangeFun, {LastSeq, Changes},
    Opts
  ),
  State#{ last_seq => LastSeq1, changes => Changes1}.
  

parse_options(Options) ->
  maps:fold(
    fun
      (include_doc, IncludeDocs, Acc) ->
        [{include_doc, IncludeDocs} | Acc];
      (history, History, Acc) ->
        [{history, History} | Acc];
      (_, _, Acc) -> Acc
    end,
    [],
    Options
  ).

encode_sse(Change) ->
  << "id: ", (integer_to_binary(Change))/binary, "\n",
     "data: ", (jsx:encode(Change))/binary >>.