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

-module(barrel_httpc).
-author("Bernard Notarianni").

-behaviour(gen_server).

-export([
         start/2,
         stop/1,
         infos/1,
         put/4,
         put_rev/5,
         get/3,
         delete/4,
         post/3,
         fold_by_id/4,
         changes_since/4,
         revsdiff/3
        ]).

-export([start_link/0]).
-export([stop/0]).

%% gen_server API
-export([init/1, handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_cast/2]).

start(Name, Store) ->
  gen_server:call(?MODULE, {start, Name, Store}).

stop(_Name) ->
  {error, not_implemented}.

infos(DbRef) ->
  {_, BarrelId} = DbRef,
  {200, R} = req(get, BarrelId),
  Info = jsx:decode(R, [return_maps]),
  {ok, Info}.

post(DbRef, Doc, _Options) ->
  {_, BarrelId} = DbRef,
  post_put(post, BarrelId, Doc).

put(DbRef, DocId, Doc, _Options) ->
  Sep = <<"/">>,
  {_, BarrelId} = DbRef,
  Url = <<BarrelId/binary, Sep/binary, DocId/binary>>,
  post_put(put, Url, Doc).

post_put(Method, Url, Doc) ->
  {Code, Reply} = req(Method, Url, Doc),
  post_put_resp(Code, Reply).

post_put_resp(404, _) ->
  {error, not_found};
post_put_resp(200, R) ->
  Reply = jsx:decode(R, [return_maps, {labels, attempt_atom}]),
  DocId = maps:get(id, Reply),
  RevId = maps:get(rev, Reply),
  true = maps:get(ok, Reply),
  {ok, DocId, RevId}.


put_rev(_Db, _DocId, _Body, _History, _Options) ->
  {error, not_implemented}.

get(DbRef, DocId, _Options) ->
  {_, BarrelId} = DbRef,
  Sep = <<"/">>,
  Url = <<BarrelId/binary, Sep/binary, DocId/binary>>,
  {Code, Reply} = req(get, Url),
  get_resp(Code, Reply).

get_resp(404, _) ->
  {error, not_found};
get_resp(200, Reply) ->
  Doc = jsx:decode(Reply, [return_maps]),
  {ok, Doc}.
  

delete(DbRef, DocId, RevId, _Options) ->
  Sep = <<"/">>,
  Rev = <<"?rev=">>,
  {_, BarrelId} = DbRef,
  Url = <<BarrelId/binary, Sep/binary, DocId/binary, Rev/binary, RevId/binary>>,
  {200, R} = req(delete, Url),
  Reply = jsx:decode(R, [return_maps, {labels, attempt_atom}]),
  DocId = maps:get(id, Reply),
  NewRevId = maps:get(rev, Reply),
  true = maps:get(ok, Reply),
  {ok, DocId, NewRevId}.

fold_by_id(_Db, _Fun, _Acc, _Opts) ->
  {error, not_implemented}.

changes_since(BarrelId, Since, Fun, Acc) ->
  gen_server:call(?MODULE, {changes_since, BarrelId, Since, Fun, Acc}).

revsdiff(_Db, _DocId, _RevIds) ->
  {error, not_implemented}.

%% ----------


-record(state, {dbid, hackney_ref, hackney_acc, first_seq, buffer=[]}).

start_link() ->
  case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid}
  end.

stop() ->
  gen_server:call(?MODULE, stop).

init(_) ->
  {ok, #state{}}.

handle_call({start, DbRef, _}, _From, State) ->
  {_, DbId} = DbRef,
  Since = 0, % TODO: pass it in parameter
  gen_server:cast(?MODULE, {longpoll, DbRef, Since}),
  {reply, ok, State#state{dbid=DbId}};

handle_call({changes_since, {_, DbId}, Since, Fun, Acc}, _From,
            #state{dbid=DbId, first_seq=Since}=S) ->
  Buf = S#state.buffer,
  Reply = fold_result(Fun, Acc, Buf),
  {reply, Reply, S#state{buffer=[]}};

handle_call({changes_since, DbRef, Since, Fun, Acc}, _From, S) ->
  ChangesSince = <<"/_changes?feed=normal&since=">>,
  SinceBin = integer_to_binary(Since),
  {_Mod, BarrelId} = DbRef,
  Url = <<BarrelId/binary, ChangesSince/binary, SinceBin/binary>>,
  {ok, 200, _Headers, Ref} = hackney:request(get, Url, [], [], []),
  {ok, Body} = hackney:body(Ref),
  Answer = jsx:decode(Body, [return_maps, {labels, attempt_atom}]),
  Changes = maps:get(results, Answer),
  Reply = fold_result(Fun, Acc, Changes),
  {reply, Reply, S};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.


handle_cast({longpoll, DbRef, Since}, S) ->
  ChangesSince = <<"/_changes?feed=longpoll&since=">>,
  SinceBin = integer_to_binary(Since),
  {_Module, BarrelId} = DbRef,
  Url = <<BarrelId/binary, ChangesSince/binary, SinceBin/binary>>,
  Opts = [async, once],
  {ok, ClientRef} = hackney:get(Url, [], <<>>, Opts),
  {noreply, S#state{dbid=BarrelId, hackney_ref=ClientRef}};

handle_cast(shutdown, State) ->
  {stop, normal, State}.

handle_info({hackney_response, _Ref, {status, 204, _Reason}}, State) ->
  {noreply, State};
handle_info({hackney_response, _Ref, {status, 200, _Reason}}, State) ->
  {noreply, State};
handle_info({hackney_response, _Ref, {headers, _Headers}}, State) ->
  {noreply, State};
handle_info({hackney_response, _Ref, <<>>}, State) ->
  {noreply, State};
handle_info({hackney_response,_Ref, Bin}, S) when is_binary(Bin) ->
  {noreply, S#state{hackney_acc=Bin}};

handle_info({hackney_response, _Ref, done}, S) ->
  BarrelId = S#state.dbid,
  Reply = S#state.hackney_acc, 
  R = jsx:decode(Reply, [return_maps, {labels, attempt_atom}]),
  Results = maps:get(results, R),
  LastSeq = maps:get(last_seq, R),
  NewBuffer = S#state.buffer ++ Results,
  DbRef = {?MODULE, BarrelId},
  ok = gen_server:cast(?MODULE, {longpoll, DbRef, LastSeq}),
  ok = barrel_event:notify(DbRef, db_updated),
  {noreply, S#state{buffer=NewBuffer}};

handle_info(_Info, State) -> {noreply, State}.

%% default gen_server callbacks
terminate(_Reason, #state{hackney_ref=undefined}) ->
  ok;
terminate(_Reason, #state{hackney_ref=Ref}) ->
  ok = hackney:close(Ref),
  ok.
  
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ----------

fold_result(Fun, Acc, Results) ->
  Folder = fun(DocInfo, A) ->
               Seq = maps:get(update_seq, DocInfo),
               Doc = {error, doc_not_fetched},
               {ok, FunResult} = Fun(Seq, DocInfo, Doc, A),
               FunResult
           end,
 lists:foldr(Folder, Acc, Results).

req(Method,Url) ->
  req(Method, Url, []).

req(Method, Url, Map) when is_map(Map) ->
  Body = jsx:encode(Map),
  req(Method, Url, Body);

req(Method, Url, Body) ->
  {ok, Code, _Headers, Ref} = hackney:request(Method, Url, [], Body, []),
  {ok, Answer} = hackney:body(Ref),
  {Code, Answer}.
