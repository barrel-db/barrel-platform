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
-export([start_link/3]).
-export([stop/0]).

%% gen_server API
-export([init/1, handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_cast/2]).



-record(st, {source, target, last_seq=0, metrics}).


start_link(Source, Target) ->
  start_link(Source, Target, []).

start_link(Source, Target, Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {Source, Target, Options}, []).

stop() ->
  gen_server:call(?MODULE, stop).

init({Source, Target, Options}) ->
  Metrics = barrel_metrics:new(),
  {ok, LastSeq, Metrics2} = replicate_change(Source, Target, 0, Metrics),
  ok = barrel_event:reg(Source),
  State = #st{source=Source,
              target=Target,
              last_seq=LastSeq,
              metrics=Metrics2},
  ok = barrel_metrics:create_task(Metrics, Options),
  barrel_metrics:update_task(Metrics),
  {ok, State}.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(shutdown, State) ->
  {stop, normal, State}.

handle_info({'$barrel_event', {_Mod, _Db}, db_updated}, S) ->
  Source = S#st.source,
  Target = S#st.target,
  Since = S#st.last_seq,
  Metrics = S#st.metrics,
  {ok, LastSeq, Metrics2} = replicate_change(Source, Target, Since, Metrics),
  NewState = S#st{last_seq=LastSeq, metrics=Metrics2},
  barrel_metrics:update_task(Metrics2),
  {noreply, NewState};

%% default source event Module=barrel_db
handle_info({'$barrel_event', DbId, db_updated}, S) when is_binary(DbId) ->
  handle_info({'$barrel_event', {barrel_db, DbId}, db_updated}, S).

%% default gen_server callback
terminate(_Reason, State) ->
  barrel_metrics:update_task(State#st.metrics),
  ok = barrel_event:unreg(),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%---------------------------------------------------

replicate_change(Source, Target, Since, Metrics) ->
  {LastSeq, Changes} = changes(Source, Since),
  Results = maps:get(<<"results">>, Changes) ,
  {ok, Metrics2} = lists:foldl(fun(C, {ok, Acc}) ->
                           sync_change(Source, Target, C, Acc)
                       end, {ok, Metrics}, Results),
  {ok, LastSeq, Metrics2}.

sync_change(Source, Target, Change, Metrics) ->
  Id = maps:get(id, Change),
  RevTree = maps:get(revtree, Change),
  CurrentRev = maps:get(current_rev, Change),
  History = history(CurrentRev, RevTree),
 
  {Doc, Metrics2} = read_doc(Source, Id, Metrics),
  Metrics3 = write_doc(Target, Id, Doc, History, Metrics2),
 
  {ok, Metrics3}.


read_doc(Source, Id, Metrics) ->
  Get = fun() -> get(Source, Id, []) end,
  case timer:tc(Get) of
    {Time, {ok, Doc}} ->
      Metrics2 = barrel_metrics:inc(docs_read, Metrics, 1),
      Metrics3 = barrel_metrics:update_times(doc_read_times, Time, Metrics2),
      {Doc, Metrics3};
    _ ->
      lager:error("replicate read error on dbid=~p for docid=~p", [Source, Id]),
      Metrics2 = barrel_metrics:inc(doc_read_failures, Metrics, 1),
      {undefined, Metrics2}
    end.    
        
write_doc(_, _, undefined, _, Metrics) ->
  Metrics;
write_doc(Target, Id, Doc, History, Metrics) ->
  PutRev = fun() -> put_rev(Target, Id, Doc, History, []) end,
  case timer:tc(PutRev) of
    {Time, {ok, _, _}} ->
      Metrics2 = barrel_metrics:inc(docs_written, Metrics, 1),
      Metrics3 = barrel_metrics:update_times(doc_write_times, Time, Metrics2),
      Metrics3;
    _ -> 
      lager:error("replicate write error on dbid=~p for docid=~p", [Target, Id]),
      barrel_metrics:inc(doc_write_failures, Metrics, 1)
  end.
      

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



