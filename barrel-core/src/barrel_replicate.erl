%% Copyright 2016, Bernard Notarianni
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

-module(barrel_replicate).
-author("Bernard Notarianni").

-behaviour(gen_server).

%% specific API
-export([start_link/2]).
-export([stop/0]).

%% gen_server API
-export([init/1, handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_cast/2]).

-record(st, {source, target, last_seq=0}).

start_link(Source, Target) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {Source, Target}, []).

stop() ->
  gen_server:call(?MODULE, stop).

init({Source, Target}) ->
  {ok, LastSeq} = replicate_change(Source, Target, 0),
  ok = subscribe(Source),
  State = #st{source=Source,
              target=Target,
              last_seq=LastSeq},
  {ok, State}.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(shutdown, State) ->
  {stop, normal, State}.

handle_info(db_updated, State) ->
  Source = State#st.source,
  Target = State#st.target,
  Since = State#st.last_seq,
  {ok, LastSeq} = replicate_change(Source, Target, Since),
  {noreply, State#st{last_seq=LastSeq}}.

%% default gen_server callback
terminate(_Reason, #st{source=Source}) ->
  ok = unsubsribe(Source),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%---------------------------------------------------

replicate_change(Source, Target, Since) ->
  {LastSeq, Changes} = changes(Source, Since),
  Results = maps:get(<<"results">>, Changes) ,
  [ sync_change (Source, Target, C) || C <- Results ],
  {ok, LastSeq}.

sync_change(Source, Target, Change) ->
  Id = maps:get(id, Change),
  RevTree = maps:get(revtree, Change),
  CurrentRev = maps:get(current_rev, Change),
  History = history(CurrentRev, RevTree),
  {SourceMod, SourceCtx} = Source,
  {ok, Doc} = SourceMod:get(SourceCtx, Id, []),
  {TargetMod, TargetCtx} = Target,
  {ok, _, _} = TargetMod:put_rev(TargetCtx, Id, Doc, History, []),
  ok.

changes(Source, Since) ->
  Fun = fun(Seq, DocInfo, _Doc, {_LastSeq, DocInfos}) ->
            {ok, {Seq, [DocInfo|DocInfos]}}
        end,
  {SourceMod, SourceCtx} = Source,
  {LastSeq, Changes} = SourceMod:changes_since(SourceCtx, Since, Fun, {Since, []}),
  {LastSeq, #{<<"last_seq">> => LastSeq,
              <<"results">> => Changes}}.

subscribe(DbRef) ->
  Key = key(DbRef),
  ok = gen_event:add_handler({via, gproc, Key}, barrel_replicate_events, self()),
  ok.

unsubsribe(DbRef) ->
  Key = key(DbRef),
  ok = gen_event:delete_handler({via, gproc, Key}, barrel_replicate_events, self()),
  ok.

%% TODO move this logic to a barrel_db:key_notify(DbName)
%% and barrel_httpc:key_notify(DbName)
%% This will avoid coupling of barrel application to barrel_httpc
%% by inverting the dependance.
key({barrel_db, DbName}) ->
  barrel_db_event:key(DbName);
key({barrel_httpc, DbName}) ->
  barrel_httpc:key_notify(DbName).
  

history(Id, RevTree) ->
  history(Id, RevTree, []).
history(<<>>, _RevTree, History) ->
  lists:reverse(History);
history(Rev, RevTree, History) ->
  DocInfo = maps:get(Rev, RevTree),
  Parent = maps:get(parent, DocInfo),
  history(Parent, RevTree, [Rev|History]).
