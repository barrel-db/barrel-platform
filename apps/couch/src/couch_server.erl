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

-module(couch_server).
-behaviour(gen_server).

-export([open/2,create/2,delete/2,get_version/0,get_version/1,get_uuid/0]).
-export([all_databases/0, all_databases/2]).
-export([init/1, handle_call/3,sup_start_link/0]).
-export([handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([dev_start/0,is_admin/2,has_admins/0,get_stats/0]).

%% hooks
-export([db_updated/2, ddoc_updated/2]).

-include("couch_db.hrl").

-record(server,{
    root_dir = [],
    dbname_regexp,
    dbs_open=0,
    start_time=""
    }).

dev_start() ->
    couch:stop(),
    up_to_date = make:all([load, debug_info]),
    couch:start().

get_version() ->
    couch:version().
get_version(short) ->
  %% strip git hash from version string
  [Version|_Rest] = string:tokens(get_version(), "+"),
  Version.


get_uuid() ->
    case couch_config:get("couchdb", "uuid", nil) of
        nil ->
            UUID = couch_uuids:random(),
            couch_config:set("couchdb", "uuid", ?b2l(UUID)),
            UUID;
        UUID -> ?l2b(UUID)
    end.

get_stats() ->
    {ok, #server{start_time=Time,dbs_open=Open}} =
            gen_server:call(couch_server, get_server),
    [{start_time, ?l2b(Time)}, {dbs_open, Open}].

sup_start_link() ->
    gen_server:start_link({local, couch_server}, couch_server, [], []).

open(DbName, Options0) ->
    Options = maybe_add_sys_db_callbacks(DbName, Options0),
    case gen_server:call(couch_server, {open, DbName, Options}, infinity) of
    {ok, Db} ->
        Ctx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
        {ok, Db#db{user_ctx=Ctx}};
    Error ->
        Error
    end.

create(DbName, Options0) ->
    Options = maybe_add_sys_db_callbacks(DbName, Options0),
    case gen_server:call(couch_server, {create, DbName, Options}, infinity) of
    {ok, Db} ->
        Ctx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
        {ok, Db#db{user_ctx=Ctx}};
    {error, eexist} ->
        file_exists;
    Error ->
        Error
    end.

delete(DbName, Options) ->
    gen_server:call(couch_server, {delete, DbName, Options}, infinity).

maybe_add_sys_db_callbacks(DbName, Options) when is_binary(DbName) ->
    maybe_add_sys_db_callbacks(?b2l(DbName), Options);
maybe_add_sys_db_callbacks(DbName, Options) ->
    case couch_config:get("replicator", "db", "_replicator") of
    DbName ->
        [
            {before_doc_update, fun couch_replicator_manager:before_doc_update/2},
            {after_doc_read, fun couch_replicator_manager:after_doc_read/2},
            sys_db | Options
        ];
    _ ->
        case couch_config:get("couch_httpd_auth", "authentication_db", "_users") of
        DbName ->
        [
            {before_doc_update, fun couch_users_db:before_doc_update/2},
            {after_doc_read, fun couch_users_db:after_doc_read/2},
            sys_db | Options
        ];
        _ ->
            Options
        end
    end.

check_dbname(#server{dbname_regexp=RegExp}, DbName) ->
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

is_admin(User, ClearPwd) ->
    case couch_config:get("admins", User) of
    "-hashed-" ++ HashedPwdAndSalt ->
        [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
        couch_util:to_hex(crypto:hash(sha, ClearPwd ++ Salt)) == HashedPwd;
    _Else ->
        false
    end.

has_admins() ->
    couch_config:get("admins") /= [].

get_full_filename(Server, DbName) ->
    filename:join([Server#server.root_dir, "./" ++ DbName ++ ".couch"]).

hash_admin_passwords() ->
    hash_admin_passwords(true).

hash_admin_passwords(Persist) ->
    lists:foreach(
        fun({User, ClearPassword}) ->
            HashedPassword = couch_passwords:hash_admin_password(ClearPassword),
            couch_config:set("admins", User, ?b2l(HashedPassword), Persist)
        end, couch_passwords:get_unhashed_admins()).


%% HOOKS
db_updated(DbName, Event) ->
    couch_event:publish(db_updated, {DbName, Event}).

ddoc_updated(DbName, Event) ->
    couch_event:publish(ddoc_updated, {DbName, Event}).


init([]) ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_sup
    % will restart us and then we will pick up the new settings.

    RootDir = couch_config:get("couchdb", "database_dir", "."),
    Self = self(),
    ok = couch_config:register(
        fun("couchdb", "database_dir") ->
            exit(Self, config_change)
        end),
    ok = couch_file:init_delete_dir(RootDir),
    hash_admin_passwords(),
    ok = couch_config:register(
        fun("admins", _Key, _Value, Persist) ->
            % spawn here so couch_config doesn't try to call itself
            spawn(fun() -> hash_admin_passwords(Persist) end)
        end, false),
    {ok, RegExp} = re:compile("^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*$"),
    ets:new(couch_dbs_by_name, [ordered_set, protected, named_table]),
    ets:new(couch_dbs_by_pid, [set, private, named_table]),
    ets:new(couch_sys_dbs, [set, private, named_table]),

    %% register db hook
    couch_hooks:add(db_updated, all, ?MODULE, db_updated, 0),
    couch_hooks:add(ddoc_updated, all, ?MODULE, ddoc_updated, 0),


    process_flag(trap_exit, true),
    {ok, #server{root_dir=RootDir,
                dbname_regexp=RegExp,
                start_time=couch_util:rfc1123_date()}}.

terminate(_Reason, _Srv) ->
    %% unregister db hooks
    couch_hooks:remove(db_updated, all, ?MODULE, db_updated, 0),
    couch_hooks:remove(ddoc_updated, all, ?MODULE, ddoc_updated, 0),

    lists:foreach(
        fun({_, Pid}) ->
                couch_util:shutdown_sync(Pid)
        end,
        ets:tab2list(couch_dbs_by_name)).

all_databases() ->
    {ok, DbList} = all_databases(
        fun(DbName, Acc) -> {ok, [DbName | Acc]} end, []),
    {ok, lists:usort(DbList)}.

all_databases(Fun, Acc0) ->
    {ok, #server{root_dir=Root}} = gen_server:call(couch_server, get_server),
    NormRoot = couch_util:normpath(Root),
    FinalAcc = try
        filelib:fold_files(Root, "^[a-z0-9\\_\\$()\\+\\-]*[\\.]couch$", true,
            fun(Filename, AccIn) ->
                NormFilename = couch_util:normpath(Filename),
                case NormFilename -- NormRoot of
                [$/ | RelativeFilename] -> ok;
                RelativeFilename -> ok
                end,
                case Fun(?l2b(filename:rootname(RelativeFilename, ".couch")), AccIn) of
                {ok, NewAcc} -> NewAcc;
                {stop, NewAcc} -> throw({stop, Fun, NewAcc})
                end
            end, Acc0)
    catch throw:{stop, Fun, Acc1} ->
         Acc1
    end,
    {ok, FinalAcc}.

do_open_db(DbName, Server, Options, {FromPid, _}) ->
    DbNameList = binary_to_list(DbName),
    case check_dbname(Server, DbNameList) of
    ok ->
        Filepath = get_full_filename(Server, DbNameList),
        case couch_db:start_link(DbName, Filepath, Options) of
        {ok, DbPid} ->
            true = ets:insert(couch_dbs_by_name, {DbName, DbPid}),
            true = ets:insert(couch_dbs_by_pid, {DbPid, DbName}),
            DbsOpen = Server#server.dbs_open + 1,
            NewServer = Server#server{dbs_open = DbsOpen},
            Reply = (catch couch_db:open_ref_counted(DbPid, FromPid)),
            case lists:member(create, Options) of
            true ->
                    couch_hooks:run(db_updated, DbName, [DbName, created]);
            false ->
                 ok
            end,
            {reply, Reply, NewServer};
        Error ->
            {reply, Error, Server}
        end;
     Error ->
        {reply, Error, Server}
     end.

handle_call(get_server, _From, Server) ->
    {reply, {ok, Server}, Server};
handle_call({open, DbName, Options}, {FromPid,_}=From, Server) ->
    case ets:lookup(couch_dbs_by_name, DbName) of
    [] ->
        do_open_db(DbName, Server, Options, From);
    [{_, MainPid}] ->
        {reply, couch_db:open_ref_counted(MainPid, FromPid), Server}
    end;
handle_call({create, DbName, Options}, From, Server) ->
    case ets:lookup(couch_dbs_by_name, DbName) of
    [] ->
        do_open_db(DbName, Server, [create | Options], From);
    [_AlreadyRunningDb] ->
        {reply, file_exists, Server}
    end;
handle_call({delete, DbName, _Options}, _From, Server) ->
    DbNameList = binary_to_list(DbName),
    case check_dbname(Server, DbNameList) of
    ok ->
        FullFilepath = get_full_filename(Server, DbNameList),
        UpdateState =
        case ets:lookup(couch_dbs_by_name, DbName) of
        [] -> false;
        [{_, Pid}] ->
            couch_util:shutdown_sync(Pid),
            true = ets:delete(couch_dbs_by_name, DbName),
            true = ets:delete(couch_dbs_by_pid, Pid),
            true
        end,
        Server2 = case UpdateState of
        true ->
            DbsOpen = case ets:member(couch_sys_dbs, DbName) of
            true ->
                true = ets:delete(couch_sys_dbs, DbName),
                Server#server.dbs_open;
            false ->
                Server#server.dbs_open - 1
            end,
            Server#server{dbs_open = DbsOpen};
        false ->
            Server
        end,

        %% Delete any leftover .compact files.  If we don't do this a subsequent
        %% request for this DB will try to open the .compact file and use it.
        couch_file:delete(Server#server.root_dir, FullFilepath ++ ".compact"),

        case couch_file:delete(Server#server.root_dir, FullFilepath) of
        ok ->
            couch_hooks:run(db_updated, DbName, [DbName, deleted]),
            {reply, ok, Server2};
        {error, enoent} ->
            {reply, not_found, Server2};
        Else ->
            {reply, Else, Server2}
        end;
    Error ->
        {reply, Error, Server}
    end.

handle_cast(Msg, _Server) ->
    exit({unknown_cast_message, Msg}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', Pid, Reason}, Server) ->
    Server2 = case ets:lookup(couch_dbs_by_pid, Pid) of
    [] -> Server;
    [{Pid, DbName}] ->
        couch_log:info("db ~s died with reason ~p", [DbName, Reason]),

        true = ets:delete(couch_dbs_by_pid, Pid),
        true = ets:delete(couch_dbs_by_name, DbName),

        case ets:lookup(couch_sys_dbs, DbName) of
        [{DbName, _}] ->
            true = ets:delete(couch_sys_dbs, DbName),
            Server;
        [] ->
            Server#server{dbs_open = Server#server.dbs_open - 1}
        end
    end,
    {noreply, Server2};
handle_info(Info, Server) ->
    {stop, {unknown_message, Info}, Server}.
