%% Copyright (c) 2017. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_store).
-author("benoitc").

-behaviour(gen_server).

%% API
-export([
  create_db/2,
  delete_db/1,
  databases/0,
  fold_databases/2
]).

-export([
  start_link/0,
  get_ref/0,
  whereis_db/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-include("barrel.hrl").

%%%===================================================================
%%% API
%%%===================================================================

create_db(DbName, Options) ->
  case whereis_db(DbName) of
    undefined ->
      case barrel_db_sup:create_db(DbName, Options) of
        {ok, DbPid} -> barrel_db:get_db(DbPid);
        {error, {already_started, DbPid}} -> barrel_db:get_db(DbPid);
        Else -> Else
      end;
    _Db ->
      {error, db_exists}
  end.

delete_db(DbName) ->
  case whereis_db(DbName) of
    undefined -> ok;
    #db{pid=DbPid} ->
      gen_server:call(DbPid, delete_db)
  end.


databases() ->
  AllDbs = fold_databases(
    fun(DbName, _Info, Acc) -> {ok, [DbName | Acc]} end,
    []
  ),
  lists:usort(AllDbs).

fold_databases(Fun, AccIn) ->
  {ok, Ref} = get_ref(),
  DbPrefix = barrel_keys:prefix(db),
  FoldFun = fun
              (DbKey, BinInfo, Acc) ->
                << _:4/binary, DbName/binary >> = DbKey,
                Info = binary_to_term(BinInfo),
                Fun(DbName, Info, Acc)
            end,
  barrel_rocksdb:fold_prefix(Ref, DbPrefix, FoldFun, AccIn, []).


whereis_db(DbName) ->
  case ets:lookup(barrel_dbs, DbName) of
    [] -> undefined;
    [Db] -> Db
  end.

get_ref() -> gen_server:call(?MODULE, get_ref).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Ref} = init_store(),
  {ok, #{ref => Ref}}.

init_store() ->
  InMemory = application:get_env(barrel, in_memory, false),
  RdbOpts = application:get_env(barrel, rocksdb_options, []),
  DbOpts = case InMemory of
             true ->
               [{create_if_missing, true}, {in_memory, true} | RdbOpts];
             false ->
               [{create_if_missing, true} | RdbOpts]
           end,
  rocksdb:open(barrel_lib:data_dir(), DbOpts).

handle_call(get_ref, _From, State = #{ ref := Ref}) ->
  {reply, {ok, Ref}, State};

handle_call(_Request, _From, State) ->
  {reply, bad_call, State}.

handle_cast(_Request, State) ->  {noreply, State}.

handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, State) ->
  case maps:find(ref, State) of
    {ok, Ref} ->
      _ = (catch rocksdb:close(Ref));
    error ->
      ok
  end,
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
