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
  create_db/1,
  create_db/2,
  delete_db/1,
  databases/0,
  fold_databases/2
]).

-export([
  start_link/0,
  get_conf/0,
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

-define(CONF_VERSION, 1).

-deprecated([create_db/2]).

%%%===================================================================
%%% API
%%%===================================================================

create_db(DbId, Config) ->
  lager:warning("barrel_db:create/2 is deprecated", []),
  create_db(Config#{ <<"database_id">> => DbId }).

create_db(Config) when is_map(Config) ->
  case maps:find(<<"database_id">>, Config) of
    error ->
      DbId = barrel_lib:uniqid(),
      gen_server:call(?MODULE, {create_db, Config#{ <<"database_id">> => DbId}});
    {ok, DbId} ->
      case whereis_db(DbId) of
        undefined -> gen_server:call(?MODULE, {create_db, Config});
        _Db ->  {error, db_exists}
      end
  end;
create_db(_) -> erlang:error(bad_database).

delete_db(DbId) ->
  case whereis_db(DbId) of
    undefined -> ok;
    _ -> gen_server:call(?MODULE, {delete_db, DbId})
  end.


%% TODO: simply return all databases
databases() ->
  AllDbs = fold_databases(
    fun(#{ <<"database_id">> := DatabaseId }, Acc) -> {ok, [DatabaseId | Acc]} end,
    []
  ),
  lists:usort(AllDbs).

fold_databases(Fun, AccIn) ->
  {ok, Conf} = get_conf(),
  fold_databases_1(maps:get(<<"databases">>, Conf, []), Fun, AccIn).


fold_databases_1([Db | Rest], Fun, Acc) ->
  case Fun(Db, Acc) of
    {ok, Acc2} -> fold_databases_1(Rest, Fun, Acc2);
    {stop, Acc2} -> Acc2;
    stop -> Acc;
    ok -> Acc
  end;
fold_databases_1([], _Fun, Acc) ->
  Acc.

whereis_db(DbId) ->
  case ets:lookup(barrel_dbs, DbId) of
    [] -> undefined;
    [Db] -> Db
  end.

get_conf() -> gen_server:call(?MODULE, get_conf).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Conf} = load_config(),
  self() ! init_dbs,
  {ok, #{conf => Conf, db_pids => #{}}}.

handle_call({create_db, Config}, _From, State) ->
  {Reply, NState} = do_create_db(Config, State),
  {reply, Reply, NState};

handle_call({delete_db, DbId}, _From, State) ->
  Reply = do_delete_db(DbId),
  {reply, Reply, State};

handle_call(get_conf, _From, State = #{ conf := Conf}) ->
  {reply, {ok, Conf}, State};

handle_call(_Request, _From, State) ->
  {reply, bad_call, State}.

handle_cast(_Request, State) ->  {noreply, State}.

handle_info({'DOWN', _MRef, process, DbPid, _Reason}, State) ->
  NState = db_is_down(DbPid, State),
  {noreply, NState};
  
handle_info(init_dbs, State) ->
  %% load databases from config
  {Loaded, State2} = load_dbs(State),
  %% get databases from sys.config, we only create dbs not already persisted
  Dbs0 = application:get_env(barrel, dbs, []),
  Dbs = lists:filter(
    fun(#{ <<"database_id">> := Id}) -> lists:member(Id, Loaded) /= true  end,
    Dbs0
  ),
  NState = maybe_create_dbs_from_conf(Dbs, State2),
  {noreply, NState};

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

do_create_db(Config, State = #{ conf := Conf, db_pids := DbPids }) ->
  DbId = maps:get(<<"database_id">>, Config),
  case barrel_db_sup:open_db(DbId, Config) of
    {ok, DbPid} ->
      #{ <<"databases">> := Dbs } = Conf,
      {ok, Db} = barrel_db:get_db(DbPid),
      Conf2 = Conf#{ <<"databases">> => [ Config | Dbs ] },
      case persist_config(Conf2) of
        ok ->
          MRef = erlang:monitor(process, DbPid),
          ets:insert(barrel_dbs, Db),
          {{ok, Config}, State#{ conf => Conf2, db_pids => DbPids#{ DbPid => {DbId, MRef} }}};
        Error ->
          lager:error(
            "error creating ~p with config ~p: ~p~n",
            [DbId, Config, Error]
          ),
          {Error, State}
      end;
    Error ->
      lager:error(
        "error creating ~p with config ~p: ~p~n",
        [DbId, Config, Error]
      ),
      {Error, State}
  end.

do_delete_db(DbId) ->
  case ets:lookup(barrel_dbs, DbId) of
    [] -> ok;
    [Db = #db{pid=Pid}] ->
      case gen_server:call(Pid, delete_db) of
        ok ->
          lager:debug(
            "database ~p marked as deleted, waiting for deletion~n",
            [DbId]
          ),
          ets:insert(barrel_dbs, Db#db{deleted=true}),
          ok;
        Error ->
          Error
      end
  end.

db_is_down(Pid, State = #{ conf := Conf, db_pids := DbPids }) ->
  case maps:take(Pid, DbPids) of
    error -> State;
    {{DbId, MRef}, DbPids2 } ->
      case ets:take(barrel_dbs, DbId) of
        [] -> State;
        [Db] ->
          
          erlang:demonitor(MRef, [flush]),
          case Db#db.deleted of
            true ->
              #{ <<"databases">> := Dbs } = Conf,
              Dbs2 = lists:filter(
                fun
                  (#{ <<"database_id">> := Id}) when Id =:= DbId -> false;
                  (_) -> true
                end,
                Dbs
              ),
              Conf2 = Conf#{ <<"databases">> => Dbs2 },
              ok = persist_config(Conf2),
              lager:info("remove database ~p from config~n", [DbId]),
              State#{ conf => Conf2, db_pids => DbPids2};
            false ->
              lager:warning("database ~p is down~n", [DbId]),
              State#{db_pids => DbPids2}
          end
      end
  end.

load_dbs(#{ conf := Conf, db_pids := DbPids} = State) ->
  #{ <<"databases">> := Dbs } = Conf,
  {Loaded, Dbs2, State2} = lists:foldl(
    fun(#{ <<"database_id">> := DbId} = Config, {Acc, Dbs1, State1}) ->
      case barrel_db:exists(DbId, Config) of
        true ->
          case barrel_db_sup:open_db(DbId, Config) of
            {ok, DbPid} ->
              #{ <<"databases">> := Dbs } = Conf,
              {ok, Db} = barrel_db:get_db(DbPid),
              MRef = erlang:monitor(process, DbPid),
              ets:insert(barrel_dbs, Db),
              lager:debug("load database ~p from config~n", [DbId]),
              {[DbId | Acc], [Config | Dbs1], State1#{ db_pids => DbPids#{ DbPid => {DbId, MRef} }}};
            Error ->
              lager:error(
                "error loading database ~p with config ~p: ~p~n",
                [DbId, Config, Error]
              ),
              {Acc, [Config | Dbs1], State1}
          end;
        false ->
          lager:info(
            "cleanup: remove database ~p from config~n",
            [DbId]
          ),
          {Acc, Dbs1, State1}
      end
    end,
    {[], [], State},
    Dbs
  ),
  
  case Dbs -- Dbs2 of
    [] -> {Loaded, State2};
    NDbs ->
      Conf2 = Conf#{ databases => NDbs },
      ok = persist_config(Conf2),
      {Loaded, State2#{ config => Conf2}}
  end.

maybe_create_dbs_from_conf([Config | Rest], State) ->
  {_, State2} = do_create_db(Config, State),
  maybe_create_dbs_from_conf(Rest, State2);
maybe_create_dbs_from_conf([], State) ->
  State.

conf_path() ->
  Path = filename:join(barrel_lib:data_dir(), "barrel_config"),
  ok = filelib:ensure_dir(Path),
  Path.
  
persist_config(Conf) ->
  file:write_file(conf_path(), jsx:encode(Conf)).

load_config() ->
  case filelib:is_regular(conf_path()) of
    true ->
      {ok, ConfBin} = file:read_file(conf_path()),
      Conf = jsx:decode(ConfBin, [return_maps]),
      {ok, Conf};
    false ->
      Conf = #{<<"version">> => ?CONF_VERSION,
               <<"node_id">> => barrel_lib:uniqid(),
               <<"databases">> => []},
      ok = persist_config(Conf),
      {ok, Conf}
  end.