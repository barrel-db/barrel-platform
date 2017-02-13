-module(barrel_peer_changes).

-export([start_listener/2,
         stop_listener/1,
         fetch_changes/1,
         parse_change/1]).


-type listener_options() :: #{
since => non_neg_integer(),
mode => binary | sse,
include_doc => true | false,
history => true | false,
change_cb => fun( (barrel_peer:change()) -> ok )
}.

-export_type([listener_options/0]).

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
-spec start_listener(Conn, ListenerOptions) -> Res when
  Conn :: barrel:conn(),
  ListenerOptions :: listener_options(),
  ListenerPid :: pid(),
  Res :: {ok, ListenerPid} | {error, any()}.
start_listener(Conn, Options) ->
  barrel_httpc_changes:start_link(Conn, Options).

%% @doc stop a change listener
-spec stop_listener(ListenerPid) -> Res when
  ListenerPid :: pid(),
  Res :: ok.
stop_listener(ListenerPid) ->
  barrel_httpc_changes:stop(ListenerPid).

%% fetch all changes received by a listener à that time.
%% Only useful when no changes callback is given.
%% Otherwise the list will always be empty.
-spec fetch_changes(ListenerPid) -> Changes when
  ListenerPid :: pid(),
  Changes :: [barrel:change()].
fetch_changes(ListenerPid) ->
  barrel_httpc_changes:changes(ListenerPid).


%% @doc parse a binary change fetched when start_listener mod is binary
-spec parse_change(binary()) -> barrel:change().
parse_change(Change) ->
  barrel_httpc_changes:parse_change(Change).
