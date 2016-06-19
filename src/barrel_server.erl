%% Copyright 2016, Benoit Chesneau
%% Copyright 2009-2014 The Apache Software Foundation
%%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(barrel_server).
-behaviour(gen_server).



%% public api
-export([open/2,
         create/2,
         delete/2]).

-export([node_info/0,
         node_id/0,
         version/0,
         node_name/0]).

-export([process_config/1]).
-export([env/0]).
-export([set_env/2, get_env/1]).


-export([all_databases/0, all_databases/2]).
-export([database_dir/0]).
-export([is_admin/2,
         has_admins/0,
         get_stats/0]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,
         handle_cast/2,code_change/3,handle_info/2,terminate/2]).

%% hooks
-export([db_updated/2, ddoc_updated/2]).
-export([config_change/3]).

-include("barrel.hrl").
-include_lib("couch_db.hrl").

-record(state, {root_dir = [],
                dbname_regexp,
                dbs_open=0,
                start_time="",
                pending = []}).

-record(pending, {db, ref, from, reqtype, clients}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------



node_info() ->

  #{ <<"name">> => <<"barrel">>,
     <<"cluster_name">> => node_name(),
     <<"uuid">> => node_id(),
     <<"version">> => #{ <<"number">> => list_to_binary(version())}
   }.

node_name() -> [Name|_] = re:split(atom_to_list(node()), "@"), Name.

node_id() -> barrel_lib:val(nodeid).

version() -> {ok, Vsn} = application:get_key(barrel, vsn), Vsn.


process_config([]) -> ok;
process_config([E | Rest]) ->
  V = get_env(E),
  barrel_lib:set(E, V),
  process_config(Rest).


env() ->
  [
    database_dir,
    view_dir,
    uri_file,
    listeners,
    start_console,
    x_forwarded_host,
    enable_cors,
    cors,
    require_valid_user,
    auth_handlers,
    auth_timeout,
    allows_persistent_cookie,
    cookie_secret
  ].


default_env(database_dir) ->
  Name = lists:concat(["Barrel.", node()]),
  filename:absname(Name);
default_env(view_dir) ->
  get_env(database_dir);
default_env(uri_file) ->
  undefined;
default_env(listeners) ->
  [];
default_env(start_console) ->
  false;
default_env(x_forwarded_host) ->
  <<"x-forwarded-host">>;
default_env(enable_cors) ->
  false;
default_env(cors) ->
  [];
default_env(require_valid_user) ->
  false;
default_env(auth_handlers) ->
  [barrel_basic_auth, barrel_cookie_auth];
default_env(cookie_secret) ->
  600;
default_env(allows_persistent_cookie) ->
  false;
default_env(auth_timeout) ->
  600.



set_env(E, Val) -> barrel_lib:set(E, Val).

get_env(E) ->
  case ?catch_val(E) of
    {'EXIT', _} -> application:get_env(barrel, E, default_env(E));
    Val -> Val
  end.


get_stats() ->
  {ok, #state{start_time=Time,dbs_open=Open}} =
  gen_server:call(barrel_server, get_state),
  [{start_time, list_to_binary(Time)}, {dbs_open, Open}].

start_link() ->
  gen_server:start_link({local, barrel_server}, barrel_server, [], []).

open(DbName, Options0) ->
  Options = maybe_add_sys_db_callbacks(DbName, Options0),
  case gen_server:call(barrel_server, {open, DbName, Options}, infinity) of
    {ok, Db} ->
      Ctx = proplists:get_value(user_ctx, Options, barrel_lib:userctx()),
      {ok, Db#db{user_ctx=Ctx}};
    Error ->
      Error
  end.

create(DbName, Options0) ->
  Options = maybe_add_sys_db_callbacks(DbName, Options0),
  case gen_server:call(barrel_server, {create, DbName, Options}, infinity) of
    {ok, Db} ->
      Ctx = proplists:get_value(user_ctx, Options, barrel_lib:userctx()),
      {ok, Db#db{user_ctx=Ctx}};
    {error, eexist} ->
      file_exists;
    Error ->
      Error
  end.

delete(DbName, Options) ->
  gen_server:call(barrel_server, {delete, DbName, Options}, infinity).


is_admin(User, ClearPwd) ->
  case barrel_config:get("admins", User) of
    "-hashed-" ++ HashedPwdAndSalt ->
      [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
      barrel_lib:to_hex(crypto:hash(sha, ClearPwd ++ Salt)) == HashedPwd;
    _Else ->
      false
  end.

has_admins() ->
  barrel_config:get("admins") /= [].


hash_admin_passwords() ->
  hash_admin_passwords(true).

hash_admin_passwords(Persist) ->
  lists:foreach(
    fun({User, ClearPassword}) ->
        HashedPassword = couch_passwords:hash_admin_password(ClearPassword),
        barrel_config:set("admins", User, HashedPassword, Persist)
    end, couch_passwords:get_unhashed_admins()).


all_databases() ->
  {ok, DbList} = all_databases(
                   fun(DbName, Acc) -> {ok, [DbName | Acc]} end, []),
  {ok, lists:usort(DbList)}.

all_databases(Fun, Acc0) ->
  {ok, #state{root_dir=Root}} = gen_server:call(barrel_server, get_state),
  NormRoot = normpath(Root),
  FinalAcc = try
               filelib:fold_files(Root, "^[a-z0-9\\_\\$()\\+\\-]*[\\.]couch$", true,
                                  fun(Filename, AccIn) ->
                                      NormFilename = normpath(Filename),
                                      case NormFilename -- NormRoot of
                                        [$/ | RelativeFilename] -> ok;
                                        RelativeFilename -> ok
                                      end,
                                      case Fun(list_to_binary(filename:rootname(RelativeFilename, ".couch")), AccIn) of
                                        {ok, NewAcc} -> NewAcc;
                                        {stop, NewAcc} -> throw({stop, Fun, NewAcc})
                                      end
                                  end, Acc0)
             catch throw:{stop, Fun, Acc1} ->
                     Acc1
             end,
  {ok, FinalAcc}.


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

database_dir() -> get_env(database_dir).

init([]) ->
  % read config and register for configuration changes

  % just stop if one of the config settings change. couch_sup
  % will restart us and then we will pick up the new settings.

  RootDir = database_dir(),
  filelib:ensure_dir(filename:join(RootDir, "dummy")),
  init_nodeid(),


  hooks:reg(config_key_update, ?MODULE, config_change, 3),
  ok = couch_file:init_delete_dir(RootDir),
  hash_admin_passwords(),
  {ok, RegExp} = re:compile("^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*$"),
  ets:new(couch_dbs_by_name, [ordered_set, protected, named_table]),
  ets:new(couch_dbs_by_pid, [set, private, named_table]),
  ets:new(couch_sys_dbs, [set, private, named_table]),

  %% register db hook
  hooks:reg(db_updated, ?MODULE, db_updated, 2),
  hooks:reg(ddoc_updated, ?MODULE, ddoc_updated, 2),


  process_flag(trap_exit, true),
  {ok, #state{root_dir = RootDir,
              dbname_regexp = RegExp,
              start_time = barrel_lib:rfc1123_date(),
              pending = []}}.

handle_call(get_state, _From, State) ->
  {reply, {ok, State}, State};
handle_call({create, _DbName, _Options}=Req, From, State) ->
  request([{Req, From}], State);
handle_call({open, _DbName, _Options}=Req, From, State) ->
  request([{Req, From}], State);
handle_call({delete, _DbName, _Options}=Req, From, State) ->
  request([{Req, From}], State);
handle_call(_, _From, State) ->
  {reply, bad_call, State}.

handle_cast(config_change, State) ->
  {stop, config_change, State};

handle_cast(Msg, _State) ->
  exit({unknown_cast_message, Msg}).


handle_info({pending_reply, {Ref, Result}}, State) ->
  {value, #pending{db = Db, from=From, clients=Clients}} =
  lists:keysearch(Ref, #pending.ref, State#state.pending),
  gen_server:reply(From, Result),
  NP = lists:keydelete(Db, #pending.db, State#state.pending),
  State1 = State#state{pending = NP},
  request(Clients, State1);

handle_info({'EXIT', Pid, Reason}, State) ->
  State2 = case ets:lookup(couch_dbs_by_pid, Pid) of
             [] -> State;
             [{Pid, DbName}] ->
               lager:info("db ~s died with reason ~p", [DbName, Reason]),

               true = ets:delete(couch_dbs_by_pid, Pid),
               true = ets:delete(couch_dbs_by_name, DbName),

               case ets:lookup(couch_sys_dbs, DbName) of
                 [{DbName, _}] ->
                   true = ets:delete(couch_sys_dbs, DbName),
                   State;
                 [] ->
                   State#state{dbs_open = State#state.dbs_open - 1}
               end
           end,
  {noreply, State2};
handle_info(Info, State) ->
  {stop, {unknown_message, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _Srv) ->
  %% unregister db hooks
  hooks:unreg(db_updated, ?MODULE, db_updated, 2),
  hooks:unreg(ddoc_updated, ?MODULE, ddoc_updated, 2),

  lists:foreach(
    fun({_, Pid}) ->
        barrel_lib:shutdown_sync(Pid)
    end,
    ets:tab2list(couch_dbs_by_name)).



%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% DB HOOKS
db_updated(DbName, Event) ->
  barrel_event:notify(DbName, Event).

ddoc_updated(DbName, Event) ->
  barrel_event:notify(DbName, Event).

%% CONFIG hooks
config_change("barrel", "database_dir", _) ->
  gen_server:cast(barrel_server, config_change);
config_change(_, _, _) ->
  ok.


maybe_add_sys_db_callbacks(DbName, Options) when is_binary(DbName) ->
  maybe_add_sys_db_callbacks(binary_to_list(DbName), Options);

maybe_add_sys_db_callbacks("_replicator", Options) ->
  [
    {before_doc_update, fun couch_replicator_manager:before_doc_update/2},
    {after_doc_read, fun couch_replicator_manager:after_doc_read/2},
    sys_db | Options
  ];
maybe_add_sys_db_callbacks("_users", Options) ->
  [
    {before_doc_update, fun couch_users_db:before_doc_update/2},
    {after_doc_read, fun couch_users_db:after_doc_read/2},
    sys_db | Options
  ];
maybe_add_sys_db_callbacks(_, Options) ->
  Options.

check_dbname(#state{dbname_regexp=RegExp}, DbName) ->
  case re:run(DbName, RegExp, [{capture, none}]) of
    nomatch ->
      case DbName of
        "_users" -> ok;
        "_replicator" -> ok;
        _Else ->
          {error, illegal_database_name, DbName}
      end;
    match ->
      ok
  end.

get_full_filename(State, DbName) ->
  filename:join([State#state.root_dir, "./" ++ DbName ++ ".couch"]).


request([{Req, From} | Rest], State) ->
  Res = case Req of
          {create, Db, Options} ->
            handle_create(State, Req, From, Db, Options);
          {open, Db, Options} ->
            handle_open(State, Req, From, Db, Options);
          {delete, Db, _Options} ->
            handle_delete(State, Req, From, Db)
        end,

  State2 = case Res of
             {pending, State1} ->
               State1;
             {Reply, State1} ->
               gen_server:reply(From, Reply),
               State1
           end,
  request(Rest, State2);
request([], State) ->
  {noreply, State}.


handle_open(State, Req, {FromPid, _} = From, Db, Options) ->
  case check_pending(Db, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      case ets:lookup(couch_dbs_by_name, Db) of
        [] ->
          do_open_db(State, From, Db, Options);
        [{_, MainPid}] ->
          {couch_db:open_ref_counted(MainPid, FromPid), State}
      end
  end.

handle_create(State, Req, From, Db, Options) ->
  case check_pending(Db, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      case ets:lookup(couch_dbs_by_name, Db) of
        [] ->
          do_open_db(State, From, Db, [create | Options]);
        [_AlreadyRunningDb] ->
          {file_exists, State}
      end
  end.

handle_delete(State, Req, From, Db) ->
  case check_pending(Db, From, State, Req) of
    {pending, NewState} -> {pending, NewState};
    false ->
      UpdateState = case ets:lookup(couch_dbs_by_name, Db) of
                      [] -> false;
                      [{_, Pid}] ->
                        barrel_lib:shutdown_sync(Pid),
                        true = ets:delete(couch_dbs_by_name, Db),
                        true = ets:delete(couch_dbs_by_pid, Pid)
                    end,
      NState = case UpdateState of
                 true ->
                   case ets:member(couch_sys_dbs, Db) of
                     true ->
                       true = ets:delete(couch_sys_dbs, Db),
                       State;
                     false ->
                       State#state{dbs_open=State#state.dbs_open - 1}
                   end;
                 false ->
                   State
               end,
      pending_call(Db, nil, make_ref(), From, internal_delete, NState)
  end.

pending_call(Db, Pid, Ref, {FromPid, _Tag}=From, ReqT, State) ->
  Server = self(),
  F = fun() ->
          Res = case ReqT of
                  internal_open ->
                    internal_open(Pid, FromPid);
                  internal_delete ->
                    internal_delete(Db, State)
                end,
          Server ! {pending_reply, {Ref, Res}}
      end,

  _ = spawn(F),
  PD = #pending{ db= Db, ref = Ref, from = From, reqtype = ReqT, clients=[]},
  P = [PD | State#state.pending],
  {pending, State#state{pending=P}}.

do_open_db(State, From, Db, Options) ->
  DbList = binary_to_list(Db),
  case check_dbname(State, DbList) of
    ok ->
      Filepath = get_full_filename(State, DbList),
      case couch_db:start_link(Db, Filepath, Options) of
        {ok, DbPid} ->
          true = ets:insert(couch_dbs_by_name, {Db, DbPid}),
          true = ets:insert(couch_dbs_by_pid, {DbPid, Db}),
          case lists:member(create, Options) of
            true ->
              hooks:run(db_updated, [Db, created]);
            false ->
              ok
          end,
          NState = State#state{dbs_open = State#state.dbs_open + 1},
          pending_call(Db, DbPid, make_ref(), From, internal_open, NState);
        Error ->
          {Error, State}
      end;
    Error ->
      {Error, State}
  end.


internal_open(DbPid, FromPid) ->
  Reply = (catch couch_db:open_ref_counted(DbPid, FromPid)),
  Reply.

internal_delete(Db, State) ->
  DbList = binary_to_list(Db),
  case check_dbname(State, DbList) of
    ok ->
      FullFilepath = get_full_filename(State, DbList),
      %% Delete any leftover .compact files.  If we don't do this a subsequent
      %% request for this DB will try to open the .compact file and use it.
      _ = couch_file:delete(State#state.root_dir, FullFilepath ++ ".compact"),
      case couch_file:delete(State#state.root_dir, FullFilepath) of
        ok ->
          hooks:run(db_updated, [Db, deleted]),
          ok;
        {error, enoent} -> not_found;
        Error -> Error
      end;
    Error ->
      Error
  end.


check_pending(Db, From, State, Req) ->
  case lists:keysearch(Db, #pending.db, State#state.pending) of
    {value, #pending{db = Db, clients=Clients}=P} ->
      NP = lists:keyreplace(Db, #pending.db, State#state.pending,
                            P#pending{clients = Clients++[{Req,From}]}),
      {pending, State#state{pending=NP}};
    false ->
      false
  end.


% Normalize a pathname by removing .. and . components.
normpath(Path) ->
  normparts(filename:split(Path), []).

normparts([], Acc) ->
  filename:join(lists:reverse(Acc));
normparts([".." | RestParts], [_Drop | RestAcc]) ->
  normparts(RestParts, RestAcc);
normparts(["." | RestParts], Acc) ->
  normparts(RestParts, Acc);
normparts([Part | RestParts], Acc) ->
  normparts(RestParts, [Part | Acc]).


init_nodeid() ->
  case init:get_argument(setnodeid) of
    {ok, [[NodeId0]]} ->
      NodeId = list_to_binary(NodeId0),
      barrel_lib:set(nodeid, NodeId);
    _ ->
      case read_nodeid() of
        {error, Error} ->
          error_logger:error_msg(Error, []),
          erlang:error(Error);
        {ok, NodeId} ->
          barrel_lib:set(nodeid, NodeId)
      end
  end.


read_nodeid() ->
  Name = filename:join(database_dir(), "NODEID"),
  case file:read_file(Name) of
    {ok, NodeId} -> {ok, NodeId};
    {error, enoent} ->
      NodeId = barrel_uuids:random(),
      case file:write_file(Name, NodeId) of
        ok -> {ok, NodeId};
        Error -> Error
      end
  end.


