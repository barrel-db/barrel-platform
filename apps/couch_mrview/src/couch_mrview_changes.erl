% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
-module(couch_mrview_changes).

-export([handle_changes/6]).

-include_lib("couch/include/couch_db.hrl").

-record(vst, {dbname,
              ddoc,
              view,
              view_options,
              queries,
              since,
              callback,
              acc,
              user_timeout,
              timeout,
              heartbeat,
              timeout_acc=0,
              notifier,
              stream,
              refresh,
              tref=nil}).

-type changes_stream() :: true | false | once.
-type changes_options() :: [{stream, changes_stream()} |
                            {since, integer()} |
                            {view_options, list()} |
                            {timeout, integer()} |
                            {heartbeat, true | integer()} |
                            {refresh, true | false}].

-export_type([changes_stream/0]).
-export_type([changes_options/0]).

%% @doc function returning changes in a streaming fashion if needed.
-spec handle_changes(binary(), binary(), binary(), function(), term(),
                     changes_options()) -> ok | {error, term()}.
handle_changes(DbName, DDocId, View, Fun, Acc, Options) ->
    Since = proplists:get_value(since, Options, 0),
    Stream = proplists:get_value(stream, Options, false),
    ViewOptions = proplists:get_value(view_options, Options, []),
    Queries = proplists:get_value(queries, Options),

    RefreshDefault = case barrel_config:get("couch_index", "refresh_index", "true") of
                         "true" -> true;
                         _ -> false
                     end,
    Refresh = proplists:get_value(refresh, Options, RefreshDefault),

    State0 = #vst{dbname=DbName,
                  ddoc=DDocId,
                  view=View,
                  view_options=ViewOptions,
                  queries=Queries,
                  since=Since,
                  callback=Fun,
                  acc=Acc,
                  refresh=Refresh},

    case view_changes_since(State0) of
        {ok, #vst{since=LastSeq, acc=Acc2}=State} ->
            case Stream of
                true ->
                    changes_loop(State#vst{stream=true}, Options);
                once when LastSeq =:= Since ->
                    changes_loop(State#vst{stream=once}, Options);
                _ ->
                    Fun(stop, {LastSeq, Acc2})
            end;
        {stop, #vst{since=LastSeq, acc=Acc2}} ->
            Fun(stop, {LastSeq, Acc2});
        Error ->
            Error
    end.

changes_loop(#vst{dbname=DbName, ddoc=DDocId, refresh=Refresh}=State0, Options) ->
    {UserTimeout, Timeout, Heartbeat} = changes_timeout(Options),

    process_flag(trap_exit, true),

    _ = couch_event:subscribe_cond(index_update, [{{'$1', DbName, DDocId, '_'},
                                                   [], ['$1']}]),

    State = State0#vst{user_timeout=UserTimeout,
                       timeout=Timeout,
                       heartbeat=Heartbeat},

    %% maybe trigger the heartbear
    TRef = case Heartbeat of
                false -> nil;
                _ ->
                    {ok, TRef1} = timer:send_interval(Heartbeat, heartbeat),
                    TRef1
           end,

    %% lock the indexer
    maybe_acquire_indexer(Refresh, DbName, DDocId),
    try
        loop(State#vst{tref=TRef})
    after
        terminate_loop(State#vst{tref=TRef})
    end.


terminate_loop(State) ->
    #vst{dbname=DbName,
         ddoc=DDocId,
         tref=TRef,
         refresh=Refresh} = State,

    maybe_release_indexer(Refresh, DbName, DDocId),

    couch_event:unsubscribe(index_update),
    case TRef of
        nil ->
            ok;
        _ ->
            timer:cancel(TRef)
    end,
    ok.


loop(#vst{since=Since, callback=Callback,
          acc=Acc, timeout=Timeout, heartbeat=Heartbeat,
          stream=Stream}=State) ->
    receive
        heartbeat ->
            case Callback(heartbeat, Acc) of
                {ok, Acc2} ->
                    loop(State#vst{acc=Acc2, timeout_acc=0});
                {stop, Acc2} ->
                    Callback(stop, {Since, Acc2})
            end;
        updated ->

            case view_changes_since(State) of
                {ok, State2} when Stream =:= true ->
                    loop(State2);
                {ok, #vst{since=LastSeq, acc=Acc2}} ->
                    Callback(stop, {LastSeq, Acc2});
                {stop, #vst{since=LastSeq, acc=Acc2}} ->
                    Callback(stop, {LastSeq, Acc2})
            end;
        reset ->
            Callback(stop, {Since, Acc});
        Message ->
            barrel_log:info("got unexpected message ~p~n", [Message]),
            exit(normal)
    after Timeout ->
              if
                  Heartbeat /= false ->
                      case Callback(heartbeat, Acc) of
                          {ok, Acc2} ->
                              loop(State#vst{acc=Acc2, timeout_acc=0});
                          {stop, Acc2} ->
                              Callback(stop, {Since, Acc2})
                      end;
                  true ->
                      Callback(stop, {Since, Acc})
              end
    end.

changes_timeout(Options) ->
    DefaultTimeout = barrel_config:get_integer("httpd", "changes_timeout", 60000),
    UserTimeout = case proplists:get_value(timeout, Options) of
                      undefined -> DefaultTimeout;
                      Timeout1 -> Timeout1
                  end,


    case UserTimeout of
        infinity -> {infinity, infinity, false};
        Timeout ->
            UserHeartbeat = proplists:get_value(heartbeat, Options),
            Heartbeat= case UserHeartbeat of
                           H when is_integer(H) ->
                               H;
                           true ->
                               DefaultTimeout;
                           _ ->
                               false
                       end,
            {Timeout, Timeout, Heartbeat}
    end.

view_changes_since(#vst{dbname=DbName, ddoc=DDocId, view=View,
                        view_options=ViewOptions, queries=Queries,
                        since=Since, callback=Callback, acc=UserAcc}=State) ->
    Wrapper = fun ({{Seq, _Key, _DocId}, _Val}=KV, {_Go, Acc2, OldSeq}) ->
            LastSeq = if OldSeq < Seq -> Seq;
                true -> OldSeq
            end,

            {Go, Acc3} = Callback(KV, Acc2),
            {Go, {Go, Acc3, LastSeq}}
    end,

    Acc0 = {ok, UserAcc, Since},
    Res = case {Queries, ViewOptions} of
        {Queries, []} when is_list(Queries) ->
            Args = {DbName, DDocId, View, Wrapper, Since},
            multi_view_changes(Queries, Args, Acc0);
        {undefined, ViewOptions} when is_list(ViewOptions) ->
             couch_mrview:view_changes_since(DbName, DDocId, View, Since,
                                             Wrapper, ViewOptions, Acc0);
        {[], []} ->
            couch_mrview:view_changes_since(DbName, DDocId, View, Since,
                                            Wrapper, [], Acc0);
        _ ->
            {error, badarg}
    end,

    case Res of
        {ok, {Go, _, Since}} ->
            timer:sleep(100),
            {Go, State};
        {ok, {Go, UserAcc2, Since2}}->
            {Go, State#vst{since=Since2, acc=UserAcc2}};
        Error ->
            Error
    end.

multi_view_changes([], _Args, Acc) ->
    {ok, Acc};
multi_view_changes([Options | Rest], {DbName, DDocId, View, Wrapper, Since}=Args,
                   Acc) ->
    case couch_mrview:view_changes_since(DbName, DDocId, View, Since,
                                         Wrapper, Options, Acc) of
        {ok, {stop, _UserAcc2, _Since2}=Acc2} ->
            {ok, Acc2};
        {ok, Acc2} ->
            multi_view_changes(Rest, Args, Acc2);
        Error ->
            Error
    end.


%% acquire the background indexing task so it can eventually be started
%% if the process close the background task will be automatically
%% released.
maybe_acquire_indexer(false, _, _) ->
    ok;
maybe_acquire_indexer(true, DbName, DDocId) ->
    couch_index_server:acquire_indexer(couch_mrview_index, DbName,
                                       DDocId).

%% release the background indexing task so it can eventually be stopped
maybe_release_indexer(false, _, _) ->
    ok;
maybe_release_indexer(true, DbName, DDocId) ->
    couch_index_server:release_indexer(couch_mrview_index, DbName,
                                       DDocId).
