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

-export([connect/2,
         disconnect/1,
         infos/1,
         put/4,
         put_rev/5,
         get/3,
         delete/4,
         post/3,
         fold_by_id/4,
         changes_since/4,
         revsdiff/3,
         write_system_doc/3,
         read_system_doc/2,
         delete_system_doc/2
        ]).

-export([start_link/2]).

%% gen_server API
-export([init/1, handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_cast/2]).

-include_lib("hackney/include/hackney_lib.hrl").

-record(state, {dbid, hackney_ref, hackney_acc, first_seq, buffer=[]}).

connect(Url, Options) ->
  case start_link(Url, Options) of
    {ok, Pid} -> {ok, {?MODULE, Pid}};
    Error -> Error
  end.

start_link(Url, Options) ->
  case gen_server:start_link(?MODULE, [Url, Options], []) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid};
    Error -> Error
  end.

disconnect(Pid) ->
  gen_server:call(Pid, stop).

infos(Pid) ->
  gen_server:call(Pid, infos).

post(Pid, Doc, Options) ->
  gen_server:call(Pid, {post, Doc, Options}).

put(Pid, DocId, Doc, Options) ->
  gen_server:call(Pid, {put, DocId, Doc, Options}).

put_rev(Pid, DocId, Doc, History, Options) ->
  gen_server:call(Pid, {put_rev, DocId, Doc, History, Options}).

get(Pid, DocId, Options) ->
  gen_server:call(Pid, {get, DocId, Options}).

delete(Pid, DocId, RevId, Options) ->
  gen_server:call(Pid, {delete, DocId, RevId, Options}).

fold_by_id(_Db, _Fun, _Acc, _Opts) ->
  {error, not_implemented}.

changes_since(Pid, Since, Fun, Acc) ->
  gen_server:call(Pid, {changes_since, Since, Fun, Acc}).

revsdiff(_Db, _DocId, _RevIds) ->
  {error, not_implemented}.

write_system_doc(Pid, DocId, Doc) ->
  gen_server:call(Pid, {write_system_doc, DocId,  Doc}).

read_system_doc(Pid, DocId) ->
  gen_server:call(Pid, {read_system_doc, DocId}).

delete_system_doc(Pid, DocId) ->
  gen_server:call(Pid, {delete_system_doc, DocId}).

%% ----------

init([Url, _Options]) ->
  Since = 0, % TODO: pass it in parameter
  State = #state{dbid=Url},
  {noreply, State2} =  handle_cast({longpoll, Url, Since}, State),
  {ok, State2}.

handle_call(infos, _From, State) ->
  DbUrl = State#state.dbid,
  {200, R} = req(get, DbUrl),
  Info = jsx:decode(R, [return_maps]),
  {reply, {ok, Info}, State};

handle_call({post, Doc, _Options}, _From, State) ->
  DbUrl = State#state.dbid,
  post_put(post, DbUrl, Doc, State);

handle_call({put, DocId, Doc, _Options}, _From, State) ->
  DbUrl = State#state.dbid,
  Sep = <<"/">>,
  Url = <<DbUrl/binary, Sep/binary, DocId/binary>>,
  post_put(put, Url, Doc, State);

handle_call({put_rev, DocId, Doc, History, _Options}, _From, State) ->
  DbUrl = State#state.dbid,
  Url = << DbUrl/binary, "/", DocId/binary, "?edit" >>,
  put_rev(Url, Doc, History, State);

handle_call({get, DocId, _Options}, _From, State) ->
  DbUrl = State#state.dbid,
  Sep = <<"/">>,
  Url = <<DbUrl/binary, Sep/binary, DocId/binary>>,
  {Code, Reply} = req(get, Url),
  get_resp(Code, Reply, State);

handle_call({delete, DocId, RevId, _Options}, _From, State) ->
  DbUrl = State#state.dbid,
  Sep = <<"/">>,
  Rev = <<"?rev=">>,
  Url = <<DbUrl/binary, Sep/binary, DocId/binary, Rev/binary, RevId/binary>>,
  {200, R} = req(delete, Url),
  Reply = jsx:decode(R, [return_maps, {labels, attempt_atom}]),
  DocId = maps:get(id, Reply),
  NewRevId = maps:get(rev, Reply),
  true = maps:get(ok, Reply),
  {reply, {ok, DocId, NewRevId}, State};

handle_call({changes_since, Since, Fun, Acc}, _From,
            #state{first_seq=Since}=S) ->
  Buf = S#state.buffer,
  Reply = fold_result(Fun, Acc, Buf),
  {reply, Reply, S#state{buffer=[]}};

handle_call({changes_since, Since, Fun, Acc}, _From, S) ->
  DbUrl = S#state.dbid,
  ChangesSince = <<"/_changes?feed=normal&since=">>,
  SinceBin = integer_to_binary(Since),
  Url = <<DbUrl/binary, ChangesSince/binary, SinceBin/binary>>,
  {ok, 200, _Headers, Ref} = hackney:request(get, Url, [], [], []),
  {ok, Body} = hackney:body(Ref),
  Answer = jsx:decode(Body, [return_maps, {labels, attempt_atom}]),
  Changes = maps:get(results, Answer),
  Reply = fold_result(Fun, Acc, Changes),
  {reply, Reply, S};

handle_call({write_system_doc, DocId, Doc}, _From, State) ->
  DbUrl = State#state.dbid,
  Sep = <<"/_system/">>,
  Url = <<DbUrl/binary, Sep/binary, DocId/binary>>,
  {Code, _} = req(put, Url, Doc),
  case lists:member(Code, [200,201]) of
    true ->
      {reply, ok, State};
    false ->
      {reply, {error, {bad_http_code, Code}}, State}
  end;

handle_call({read_system_doc, DocId}, _From, State) ->
  DbUrl = State#state.dbid,
  Sep = <<"/_system/">>,
  Url = <<DbUrl/binary, Sep/binary, DocId/binary>>,
  {Code, Reply} = req(get, Url),
  get_resp(Code, Reply, State);

handle_call({delete_system_doc, DocId}, _From, State) ->
  DbUrl = State#state.dbid,
  Sep = <<"/_system/">>,
  Url = <<DbUrl/binary, Sep/binary, DocId/binary>>,
  {204, _} = req(delete, Url),
  {reply, ok, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.


handle_cast({longpoll, DbUrl, Since}, S) ->
  ChangesSince = <<"/_changes?feed=longpoll&heartbeat=5000&since=">>,
  SinceBin = integer_to_binary(Since),
  Url = <<DbUrl/binary, ChangesSince/binary, SinceBin/binary>>,
  Opts = [async, {recv_timeout, 300000}],
  {ok, ClientRef} = hackney:get(Url, [], <<>>, Opts),
  EmptyAcc = <<>>,
  {noreply, S#state{dbid=DbUrl, hackney_ref=ClientRef, hackney_acc=EmptyAcc}};

handle_cast(shutdown, State) ->
  {stop, normal, State}.

handle_info({hackney_response, _Ref, {status, 204, _Reason}}, State) ->
  {noreply, State};
handle_info({hackney_response, _Ref, {status, 200, _Reason}}, State) ->
  {noreply, State};
handle_info({hackney_response, _Ref, {headers, _Headers}}, State) ->
  {noreply, State};
handle_info({hackney_response, _Ref, <<"\n">>}, State) ->
  {noreply, State};
handle_info({hackney_response,_Ref, Bin}, S) when is_binary(Bin) ->
  Acc = S#state.hackney_acc,
  Acc2 = <<Acc/binary, Bin/binary>>,
  {noreply, S#state{hackney_acc=Acc2}};

handle_info({hackney_response, _Ref, done}, #state{hackney_acc= <<>>}=S) ->
  {noreply, S};
handle_info({hackney_response, _Ref, done}, S) ->
  DbUrl = S#state.dbid,
  Reply = S#state.hackney_acc,
  R = jsx:decode(Reply, [return_maps, {labels, attempt_atom}]),
  Results = maps:get(results, R),
  LastSeq = maps:get(last_seq, R),
  NewBuffer = S#state.buffer ++ Results,
  ok = gen_server:cast(self(), {longpoll, DbUrl, LastSeq}),
  ok = barrel_event:notify({?MODULE, self()}, db_updated),
  EmptyAcc = <<>>,
  {noreply, S#state{buffer=NewBuffer, hackney_acc=EmptyAcc}};

handle_info(_Info, State) -> {noreply, State}.

%% default gen_server callbacks
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =============================================================================
%% Process HTTP requests and construct erlang responses
%% =============================================================================

post_put(Method, Url, Doc, State) ->
  {Code, Reply} = req(Method, Url, Doc),
  post_put_resp(Code, Reply, State).

post_put_resp(404, _, State) ->
  {reply, {error, not_found}, State};
post_put_resp(200, R, State) ->
  post_put_resp(201, R, State);
post_put_resp(201, R, State) ->
  Answer = jsx:decode(R, [return_maps, {labels, attempt_atom}]),
  DocId = maps:get(id, Answer),
  RevId = maps:get(rev, Answer),
  true = maps:get(ok, Answer),
  Reply = {ok, DocId, RevId},
  {reply, Reply, State};
post_put_resp(Code, _, State) ->
  {reply, {error, {unexepected_http_code, Code}}, State}.

%% -----------------------------------------------------------------------------

put_rev(Url, Doc, History, State) ->
  Request = #{<<"document">> => Doc,
              <<"history">> => History},
  {Code, Reply} = req(put, Url, Request),
  put_rev_resp(Code, Reply, State).

put_rev_resp(404, _, State) ->
  {reply, {error, not_found}, State};

put_rev_resp(201, R, State) ->
  Answer = jsx:decode(R, [return_maps]),
  DocId = maps:get(<<"id">>, Answer),
  RevId = maps:get(<<"rev">>, Answer),
  Reply = {ok, DocId, RevId},
  {reply, Reply, State}.

%% -----------------------------------------------------------------------------

get_resp(404, _, State) ->
  {reply, {error, not_found}, State};

get_resp(200, Reply, State) ->
  Doc = jsx:decode(Reply, [return_maps]),
  {reply, {ok, Doc}, State}.

%% -----------------------------------------------------------------------------

fold_result(Fun, Acc, Results) ->
  Folder = fun(Change, A) ->
               Seq = maps:get(seq, Change),
               {ok, FunResult} = Fun(Seq, Change, A),
               FunResult
           end,
 lists:foldr(Folder, Acc, Results).

%% =============================================================================
%% Internal helpers
%% =============================================================================

req(Method,Url) ->
  req(Method, Url, []).

req(Method, Url, Map) when is_map(Map) ->
  Body = jsx:encode(Map),
  req(Method, Url, Body);

req(Method, Url, Body) ->
  ParsedUrl = hackney_url:parse_url(Url),
  PoolName = pool_name(ParsedUrl),
  Options = [{timeout, 150000}, {max_connections, 20}, {pool, PoolName}],
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  {ok, Code, _Headers, Ref} = hackney:request(Method, ParsedUrl, Headers, Body, Options),
  {ok, Answer} = hackney:body(Ref),
  {Code, Answer}.


pool_name(ParsedUrl) ->
  #hackney_url{netloc = NetLoc} = ParsedUrl,
  PoolName = <<"barrel-httpc-", NetLoc/binary>>,
  barrel_lib:to_atom(PoolName).
