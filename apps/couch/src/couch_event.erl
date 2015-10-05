%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% 2015 (c) Ulf Wiger <ulf@wiger.net>
%%
%% @doc couch publish/subscribe pattern
%% This module implements a few convenient functions for publish/subscribe.
%%
%% This code is based on gproc_ps code. Only difference is that we are
%% sending {couch_event, Event, Msg} instead of {gproc_ps_event, Event, Msg}.

-module(couch_event).

-export([publish/2,
         publish_all/2,
         subscribe/1,
         subscribe_cond/2,
         change_cond/2,
         unsubscribe/1,
         list_subs/1]).

-export([publish_db_update/2,
         subscribe_db_updates/1,
         unsubscribe_db_updates/1,
         change_db/1]).

-export([key_for_event/1]).

-define(EVTAG, couch_event).

-type event()  :: any().
-type msg()    :: any().


-spec publish(event(), msg()) -> ok.
%% @doc Publish the message `Msg' to all subscribers of `Event'
%%
%% The message delivered to each subscriber will be of the form:
%%
%% `{couch_event, Event, Msg}'
%%
%% The message will be delivered to each subscriber provided their respective
%% condition tests succeed.
%% @end
publish(Event, Msg) ->
    Message = {?EVTAG, Event, Msg},
    lists:foreach(
      fun({Pid, undefined}) ->
              Pid ! Message;
         ({Pid, Spec}) ->
              try   C = ets:match_spec_compile(Spec),
                    case ets:match_spec_run([Msg], C) of
                        [true] -> Pid ! Message;
                        _Else -> ok
                    end
              catch
                  error:_ -> ok
              end
      end, gproc:select({l,p}, [{{key_for_event(Event), '$1', '$2'},
                                  [], [{{'$1','$2'}}] }])).

%% @doc publish an event to all
publish_all(Event, Msg) ->
    gproc:reg({p, l,{?EVTAG, Event, Msg}}).

-spec subscribe(event()) -> true.
%% @doc Subscribe to events of type `Event'
%%
%% Any messages published with `couch_event:publish(Event, Msg)' will be
%% delivered to the current process, along with all other subscribers.
%%
%% This function creates a property, `{p, l, {couch_event,Event}}', which
%% can be searched and displayed for debugging purposes.
%% @end
subscribe(Event) ->
    gproc:reg(key_for_event(Event)).


-spec subscribe_cond(event(), undefined | ets:match_spec()) -> true.
%% @doc Subscribe conditionally to events of type `Event'
%%
%% This function is similar to {@link subscribe/1}, but adds a condition
%% in the form of a match specification. A spefication should
%% @end
subscribe_cond(Event, Spec) ->
    case Spec of
        undefined -> ok;
        [_|_] -> _ = ets:match_spec_compile(Spec);  % validation
        _ -> error(badarg)
    end,
    gproc:reg(key_for_event(Event), Spec).

publish_db_update(DbName, Msg) ->
    publish(db_updated, {DbName, Msg}).

%% @doc subscribe to updates of a specific database
subscribe_db_updates(DbName) when is_list(DbName) ->
    subscribe_db_updates(list_to_binary(DbName));
subscribe_db_updates(DbName) ->
    subscribe_cond(db_updated, [{{DbName, '_'}, [], [true]}]).

%% unsubscribe for db updates
unsubscribe_db_updates(_DbName) ->
    unsubscribe(db_updated).

%% @doc change db subscription for the current process
change_db(DbName) when is_list(DbName) ->
    change_db(list_to_binary(DbName));
change_db(DbName) ->
    change_cond(db_updated, [{{DbName, '_'}, [], [true]}]).

-spec change_cond(event(), undefined | ets:match_spec()) -> true.
%% @doc Change the condition specification of an existing subscription.
%%
%% This function atomically changes the condition spec of an existing
%% subscription (see {@link subscribe_cond/3}). An exception is raised if
%% the subscription doesn't already exist.
%% @end
change_cond(Event, Spec) ->
    case Spec of
        undefined -> ok;
        [_|_] -> _ = ets:match_spec_compile(Spec);  % validation
        _ -> error(badarg)
    end,
    gproc:set_value(key_for_event(Event), Spec).

-spec unsubscribe(event()) -> true.
%% @doc Remove subscribtion created using `subscribe(Event)'
%%
%% This removes the property created through `subscribe/2'.
%% @end
unsubscribe(Event) ->
    gproc:unreg(key_for_event(Event)).

-spec list_subs(event()) -> [pid()].
%% @doc List the pids of all processes subscribing to `Event'
%%
%% This function uses `gproc:select/2' to find all properties indicating a subscription.
%% @end
list_subs(Event)  ->
    gproc:select({l,p}, [{ {key_for_event(Event), '$1', '_'}, [], ['$1'] }]).

key_for_event(Event) ->
    {p, l, {?EVTAG, Event}}.
