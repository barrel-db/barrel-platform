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
  ok = barrel_event:reg(Source),
  State = #st{source=Source,
              target=Target,
              last_seq=LastSeq},
  {ok, State}.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(shutdown, State) ->
  {stop, normal, State}.

handle_info({'$barrel_event', {_Mod, _Db}, db_updated}, S) ->
  Source = S#st.source,
  Target = S#st.target,
  Since = S#st.last_seq,
  {ok, LastSeq} = replicate_change(Source, Target, Since),
  {noreply, S#st{last_seq=LastSeq}};

%% default source event Module=barrel_db
handle_info({'$barrel_event', DbId, db_updated}, S) when is_binary(DbId) ->
  handle_info({'$barrel_event', {barrel_db, DbId}, db_updated}, S).

%% default gen_server callback
terminate(_Reason, _State) ->
  ok = barrel_event:unreg(),
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
  {ok, Doc} = get(Source, Id, []),
  {ok, _, _} = put_rev(Target, Id, Doc, History, []),
  ok.


changes(Source, Since) ->
  Fun = fun(Seq, DocInfo, _Doc, {_LastSeq, DocInfos}) ->
            {ok, {Seq, [DocInfo|DocInfos]}}
        end,
  {LastSeq, Changes} = changes_since(Source, Since, Fun, {Since, []}),
  {LastSeq, #{<<"last_seq">> => LastSeq,
              <<"results">> => Changes}}.

get({Mod,_Db}=DbRef, Id, Opts) ->
  Mod:get(DbRef, Id, Opts);
get(Db, Id, Opts) when is_binary(Db) ->
  barrel_db:get(Db, Id, Opts).

put_rev({Mod,_Db}=DbRef, Id, Doc, History, Opts) ->
  Mod:put_rev(DbRef, Id, Doc, History, Opts);
put_rev(Db, Id, Doc, History, Opts) when is_binary(Db) ->
  barrel_db:put_rev(Db, Id, Doc, History, Opts).

changes_since({Mod,_Db}=DbRef, Since, Fun, Acc) ->
  Mod:changes_since(DbRef, Since, Fun, Acc);
changes_since(Db, Since, Fun, Acc) when is_binary(Db) ->
  barrel_db:changes_since(Db, Since, Fun, Acc).


history(Id, RevTree) ->
  history(Id, RevTree, []).
history(<<>>, _RevTree, History) ->
  lists:reverse(History);
history(Rev, RevTree, History) ->
  DocInfo = maps:get(Rev, RevTree),
  Parent = maps:get(parent, DocInfo),
  history(Parent, RevTree, [Rev|History]).
