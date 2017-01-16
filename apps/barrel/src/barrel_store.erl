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
  open_db/2,
  delete_db/1,
  databases/0,
  fold_databases/2
]).

-export([
  start_link/0,
  get_ref/0,
  i_list_dbs/1,
  i_fold_dbs/3,
  whereis_db/1,
  open_db_int/1
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

create_db(DbName, Options0) ->
  case whereis_db(DbName) of
    undefined ->
      DbId = barrel_keys:db_id(DbName),
      Options1 = Options0#{ create_if_missing => true},
      case barrel_db_sup:start_db(DbName, DbId, Options1) of
        {ok, DbPid} -> barrel_db:get_db(DbPid);
        {error, {already_started, DbPid}} -> barrel_db:get_db(DbPid);
        Else -> Else
      end;
    _Db ->
      {error, db_exists}
  end.


open_db(DbName, Options) ->
  case whereis_db(DbName) of
    undefined -> {error, not_found};
    #db{pid=nil}=Db->
      case barrel_db_sup:start_db(Db#db.name, Db#db.id, Options) of
        {ok, DbPid} -> barrel_db:get_db(DbPid);
        {error, {already_started, DbPid}} -> barrel_db:get_db(DbPid);
        Else -> Else
      end;
    Db ->
      {ok, Db}
  end.


open_db_int(#db{}=Db) ->
  case barrel_db_sup:start_db(Db#db.name, Db#db.id, Db#db.options) of
    {ok, DbPid} -> barrel_db:get_db(DbPid);
    {error, {already_started, DbPid}} -> barrel_db:get_db(DbPid);
    Else -> Else
  end.

delete_db(DbName) ->
  case whereis_db(DbName) of
    undefined -> ok;
    #db{pid=DbPid} ->
      gen_server:call(DbPid, delete_db)
  end.

databases() ->
  AllDbs = fold_databases(
    fun(DbName, _Db, Acc) -> [DbName | Acc] end,
    []
  ),
  lists:usort(AllDbs).

fold_databases(Fun, AccIn) ->
  ets:foldl(
    fun(Db, Acc) -> Fun(Db#db.name, Db, Acc) end,
    AccIn,
    barrel_dbs
  ).


get_ref() -> gen_server:call(?MODULE, get_ref).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Ref} = init_store(),
  %% we load the list of databases in memory
  ok = init_dbs(Ref),
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

init_dbs(Ref) ->
  Fun = fun
          (DbName, DbMeta, Acc) ->
            {DbId, DbOptions} = binary_to_term(DbMeta),
            ets:insert(barrel_dbs, #db{name=DbName, id=DbId, options=DbOptions}),
            {ok, Acc}
        end,
  i_fold_dbs(Ref, Fun, ok).

handle_call(get_ref, _From, State = #{ ref := Ref}) ->
  {reply, {ok, Ref}, State};

handle_call(_Request, _From, State) ->
  {reply, bad_call, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  lager:info("terminate ~p~n", [_Reason]),
  case maps:find(ref, State) of
    {ok, Ref} ->
      _ = (catch rocksdb:close(Ref));
    error ->
      ok
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

whereis_db(DbName) ->
  case ets:lookup(barrel_dbs, DbName) of
    [] -> undefined;
    [Db] -> Db
  end.

i_list_dbs(Ref) ->
  Fun = fun(DbName, _, Acc) -> {ok, [DbName | Acc]} end,
  AllDbs = i_fold_dbs(Ref, Fun, []),
  lists:usort(AllDbs).

i_fold_dbs(Ref, Fun, AccIn) ->
  DbPrefix = barrel_keys:prefix(db),
  FoldFun = fun
          (DbKey, DbId, Acc) ->
            << _:4/binary, DbName/binary >> = DbKey,
            Fun(DbName, DbId, Acc)
        end,
  barrel_rocksdb:fold_prefix(Ref, DbPrefix, FoldFun, AccIn, []).
