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

-module(couch_auth_cache).
-behaviour(gen_server).

% public API
-export([get_user_creds/1]).
-export([start_link/0]).
-export([handle_config_change/3]).

% gen_server API
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).

-include("couch_db.hrl").
-include("couch_js_functions.hrl").

-define(STATE, auth_state_ets).
-define(BY_USER, auth_by_user_ets).
-define(BY_ATIME, auth_by_atime_ets).


-define(DEFAULT_USERDB, <<"_users">>).
-define(DEFAULT_CACHE_SIZE, 50).

-record(state, {
    max_cache_size = 0,
    cache_size = 0,
    db_notifier = nil,
    db_mon_ref = nil
}).


-spec get_user_creds(UserName::string() | binary()) ->
    Credentials::list() | nil.

get_user_creds(UserName) when is_list(UserName) ->
    get_user_creds(list_to_binary(UserName));

get_user_creds(UserName) ->
    UserCreds = case barrel_config:get("admins", binary_to_list(UserName)) of
    "-hashed-" ++ HashedPwdAndSalt ->
        % the name is an admin, now check to see if there is a user doc
        % which has a matching name, salt, and password_sha
        [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
        case get_from_cache(UserName) of
        nil ->
            make_admin_doc(HashedPwd, Salt, []);
        UserProps when is_list(UserProps) ->
            make_admin_doc(HashedPwd, Salt, couch_util:get_value(<<"roles">>, UserProps))
        end;
    "-pbkdf2-" ++ HashedPwdSaltAndIterations ->
        [HashedPwd, Salt, Iterations] = string:tokens(HashedPwdSaltAndIterations, ","),
        case get_from_cache(UserName) of
        nil ->
            make_admin_doc(HashedPwd, Salt, Iterations, []);
        UserProps when is_list(UserProps) ->
            make_admin_doc(HashedPwd, Salt, Iterations, couch_util:get_value(<<"roles">>, UserProps))
    end;
    _Else ->
        get_from_cache(UserName)
    end,
    validate_user_creds(UserCreds).

make_admin_doc(HashedPwd, Salt, ExtraRoles) ->
    [{<<"roles">>, [<<"_admin">>|ExtraRoles]},
     {<<"salt">>, list_to_binary(Salt)},
     {<<"password_scheme">>, <<"simple">>},
     {<<"password_sha">>, list_to_binary(HashedPwd)}].

make_admin_doc(DerivedKey, Salt, Iterations, ExtraRoles) ->
    [{<<"roles">>, [<<"_admin">>|ExtraRoles]},
     {<<"salt">>, list_to_binary(Salt)},
     {<<"iterations">>, list_to_integer(Iterations)},
     {<<"password_scheme">>, <<"pbkdf2">>},
     {<<"derived_key">>, list_to_binary(DerivedKey)}].

get_from_cache(UserName) ->
    exec_if_auth_db(
        fun(_AuthDb) ->
            maybe_refresh_cache(),
            case ets:lookup(?BY_USER, UserName) of
            [] ->
                gen_server:call(?MODULE, {fetch, UserName}, infinity);
            [{UserName, {Credentials, _ATime}}] ->
                exometer:update([barrel, auth_cache_hits], 1),
                gen_server:cast(?MODULE, {cache_hit, UserName}),
                Credentials
            end
        end,
        nil
    ).


validate_user_creds(nil) ->
    nil;
validate_user_creds(UserCreds) ->
    case couch_util:get_value(<<"_conflicts">>, UserCreds) of
    undefined ->
        ok;
    _ConflictList ->
        throw({unauthorized,
            <<"User document conflicts must be resolved before the document",
              " is used for authentication purposes.">>
        })
    end,
    UserCreds.


init_hooks() ->
    hooks:reg(config_key_update, ?MODULE, handle_config_change, 3).

unregister_hooks() ->
    hooks:unreg(config_key_update, ?MODULE, handle_config_change, 3).


handle_config_change("couch_httpd_auth", "auth_cache_size", _Type) ->
    Size = barrel_config:get_integer("couch_httpd_auth", "auth_cache_size", ?DEFAULT_CACHE_SIZE),
    ok = gen_server:call(?MODULE, {new_max_cache_size, Size});
handle_config_change("couch_httpd_auth", "authentication_db", _Type) ->
    ok = gen_server:call(?MODULE, reinit_cache);
handle_config_change(_Section, _Key, _Type) ->
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    ?STATE = ets:new(?STATE, [set, protected, named_table]),
    ?BY_USER = ets:new(?BY_USER, [set, protected, named_table]),
    ?BY_ATIME = ets:new(?BY_ATIME, [ordered_set, private, named_table]),
    process_flag(trap_exit, true),
    init_hooks(),
    CacheSize = barrel_config:get_integer("couch_httpd_auth", "auth_cache_size", ?DEFAULT_CACHE_SIZE),
    AuthDbName = barrel_config:get_binary("couch_httpd_auth", "authentication_db", ?DEFAULT_USERDB),
    _ = couch_event:subscribe_db_updates(AuthDbName),
    {ok, reinit_cache(#state{max_cache_size = CacheSize})}.


handle_call(auth_db_compacted, _From, State) ->
    exec_if_auth_db(
        fun(AuthDb) ->
            true = ets:insert(?STATE, {auth_db, reopen_auth_db(AuthDb)})
        end
    ),
    {reply, ok, State};



handle_call({fetch, UserName}, _From, State) ->
    {Credentials, NewState} = case ets:lookup(?BY_USER, UserName) of
    [{UserName, {Creds, ATime}}] ->
        exometer:update([barrel, auth_cache_hits], 1),
        cache_hit(UserName, Creds, ATime),
        {Creds, State};
    [] ->
        exometer:update([barrel, auth_cache_misses], 1),
        Creds = get_user_props_from_db(UserName),
        State1 = add_cache_entry(UserName, Creds, os:timestamp(), State),
        {Creds, State1}
    end,
    {reply, Credentials, NewState};

handle_call(refresh, _From, State) ->
    exec_if_auth_db(fun refresh_entries/1),
    {reply, ok, State};

handle_call({new_max_cache_size, NewSize}, _From, #state{cache_size = Size} = State) when NewSize >= Size ->
    {reply, ok, State#state{max_cache_size = NewSize}};

handle_call({new_max_cache_size, NewSize}, _From, State) ->
    free_mru_cache_entries(State#state.cache_size - NewSize),
    {reply, ok, State#state{max_cache_size = NewSize, cache_size = NewSize}};

handle_call(reinit_cache, _From, State) ->
    catch erlang:demonitor(State#state.db_mon_ref, [flush]),
    exec_if_auth_db(fun(AuthDb) -> catch couch_db:close(AuthDb) end),
    {reply, ok, reinit_cache(State)}.

handle_cast({cache_hit, UserName}, State) ->
    case ets:lookup(?BY_USER, UserName) of
    [{UserName, {Credentials, ATime}}] ->
        cache_hit(UserName, Credentials, ATime);
    _ ->
        ok
    end,
    {noreply, State}.

handle_info({couch_event, db_updated, {_, Event}}, State) ->
    case Event of
        created ->
            catch erlang:demonitor(State#state.db_mon_ref, [flush]),
            exec_if_auth_db(fun(AuthDb) -> catch couch_db:close(AuthDb) end),
            {noreply, reinit_cache(State)};
        compacted ->
            exec_if_auth_db(
              fun(AuthDb) ->
                      true = ets:insert(?STATE, {auth_db, reopen_auth_db(AuthDb)})
              end),
            {noreply, State};
        deleted ->
            {noreply, reinit_cache(State)};
        _Else   ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    unregister_hooks(),
    [{auth_db_name, DbName}] = ets:lookup(?STATE, auth_db_name),
    catch couch_event:unsubscribe_db_updates(DbName),
    exec_if_auth_db(fun(AuthDb) -> catch couch_db:close(AuthDb) end),
    true = ets:delete(?BY_USER),
    true = ets:delete(?BY_ATIME),
    true = ets:delete(?STATE).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



clear_cache(State) ->
    exec_if_auth_db(fun(AuthDb) -> catch couch_db:close(AuthDb) end),
    true = ets:delete_all_objects(?BY_USER),
    true = ets:delete_all_objects(?BY_ATIME),
    State#state{cache_size = 0}.


reinit_cache(State) ->
    NewState = clear_cache(State),
    AuthDbName = barrel_config:get_binary("couch_httpd_auth", "authentication_db", ?DEFAULT_USERDB),
    catch _ = couch_event:change_db(AuthDbName),
    true = ets:insert(?STATE, {auth_db_name, AuthDbName}),
    AuthDb = open_auth_db(),
    true = ets:insert(?STATE, {auth_db, AuthDb}),
    NewState#state{db_mon_ref = couch_db:monitor(AuthDb)}.


add_cache_entry(_, _, _, #state{max_cache_size = 0} = State) ->
    State;
add_cache_entry(UserName, Credentials, ATime, State) ->
    case State#state.cache_size >= State#state.max_cache_size of
    true ->
        free_mru_cache_entry();
    false ->
        ok
    end,
    true = ets:insert(?BY_ATIME, {ATime, UserName}),
    true = ets:insert(?BY_USER, {UserName, {Credentials, ATime}}),
    State#state{cache_size = couch_util:get_value(size, ets:info(?BY_USER))}.

free_mru_cache_entries(0) ->
    ok;
free_mru_cache_entries(N) when N > 0 ->
    free_mru_cache_entry(),
    free_mru_cache_entries(N - 1).

free_mru_cache_entry() ->
    MruTime = ets:last(?BY_ATIME),
    [{MruTime, UserName}] = ets:lookup(?BY_ATIME, MruTime),
    true = ets:delete(?BY_ATIME, MruTime),
    true = ets:delete(?BY_USER, UserName).


cache_hit(UserName, Credentials, ATime) ->
    NewATime = os:timestamp(),
    true = ets:delete(?BY_ATIME, ATime),
    true = ets:insert(?BY_ATIME, {NewATime, UserName}),
    true = ets:insert(?BY_USER, {UserName, {Credentials, NewATime}}).


refresh_entries(AuthDb) ->
    case reopen_auth_db(AuthDb) of
    nil ->
        ok;
    AuthDb2 ->
        case AuthDb2#db.update_seq > AuthDb#db.update_seq of
        true ->
            {ok, _, _} = couch_db:enum_docs_since(
                AuthDb2,
                AuthDb#db.update_seq,
                fun(DocInfo, _, _) -> refresh_entry(AuthDb2, DocInfo) end,
                AuthDb#db.update_seq,
                []
            ),
            true = ets:insert(?STATE, {auth_db, AuthDb2});
        false ->
            ok
        end
    end.


refresh_entry(Db, #doc_info{high_seq = DocSeq} = DocInfo) ->
    case is_user_doc(DocInfo) of
    {true, UserName} ->
        case ets:lookup(?BY_USER, UserName) of
        [] ->
            ok;
        [{UserName, {_OldCreds, ATime}}] ->
            {ok, Doc} = couch_db:open_doc(Db, DocInfo, [conflicts, deleted]),
            NewCreds = user_creds(Doc),
            true = ets:insert(?BY_USER, {UserName, {NewCreds, ATime}})
        end;
    false ->
        ok
    end,
    {ok, DocSeq}.


user_creds(#doc{deleted = true}) ->
    nil;
user_creds(#doc{} = Doc) ->
    {Creds} = couch_doc:to_json_obj(Doc, []),
    Creds.


is_user_doc(#doc_info{id = <<"org.couchdb.user:", UserName/binary>>}) ->
    {true, UserName};
is_user_doc(_) ->
    false.


maybe_refresh_cache() ->
    case cache_needs_refresh() of
    true ->
        ok = gen_server:call(?MODULE, refresh, infinity);
    false ->
        ok
    end.


cache_needs_refresh() ->
    exec_if_auth_db(
        fun(AuthDb) ->
            case reopen_auth_db(AuthDb) of
            nil ->
                false;
            AuthDb2 ->
                AuthDb2#db.update_seq > AuthDb#db.update_seq
            end
        end,
        false
    ).


reopen_auth_db(AuthDb) ->
    case (catch couch_db:reopen(AuthDb)) of
    {ok, AuthDb2} ->
        AuthDb2;
    _ ->
        nil
    end.


exec_if_auth_db(Fun) ->
    exec_if_auth_db(Fun, ok).

exec_if_auth_db(Fun, DefRes) ->
    case ets:lookup(?STATE, auth_db) of
    [{auth_db, #db{} = AuthDb}] ->
        Fun(AuthDb);
    _ ->
        DefRes
    end.


open_auth_db() ->
    [{auth_db_name, DbName}] = ets:lookup(?STATE, auth_db_name),
    {ok, AuthDb} = ensure_users_db_exists(DbName, [sys_db]),
    AuthDb.


get_user_props_from_db(UserName) ->
    exec_if_auth_db(
        fun(AuthDb) ->
            Db = reopen_auth_db(AuthDb),
            DocId = <<"org.couchdb.user:", UserName/binary>>,
            try
                {ok, Doc} = couch_db:open_doc(Db, DocId, [conflicts]),
                {DocProps} = couch_doc:to_json_obj(Doc, []),
                DocProps
            catch
            _:_Error ->
                nil
            end
        end,
        nil
    ).

ensure_users_db_exists(DbName, Options) ->
    Options1 = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}, nologifmissing | Options],
    case couch_db:open(DbName, Options1) of
    {ok, Db} ->
        ensure_auth_ddoc_exists(Db, <<"_design/_auth">>),
        {ok, Db};
    _Error ->
        {ok, Db} = couch_db:create(DbName, Options1),
        ok = ensure_auth_ddoc_exists(Db, <<"_design/_auth">>),
        {ok, Db}
    end.

ensure_auth_ddoc_exists(Db, DDocId) ->
    case couch_db:open_doc(Db, DDocId) of
    {not_found, _Reason} ->
        {ok, AuthDesign} = auth_design_doc(DDocId),
        {ok, _Rev} = couch_db:update_doc(Db, AuthDesign, []);
    {ok, Doc} ->
        {Props} = couch_doc:to_json_obj(Doc, []),
        case couch_util:get_value(<<"validate_doc_update">>, Props, []) of
            ?AUTH_DB_DOC_VALIDATE_FUNCTION ->
                ok;
            _ ->
                Props1 = lists:keyreplace(<<"validate_doc_update">>, 1, Props,
                    {<<"validate_doc_update">>,
                    ?AUTH_DB_DOC_VALIDATE_FUNCTION}),
                couch_db:update_doc(Db, couch_doc:from_json_obj({Props1}), [])
        end
    end,
    ok.

auth_design_doc(DocId) ->
    DocProps = [
        {<<"_id">>, DocId},
        {<<"language">>,<<"javascript">>},
        {<<"validate_doc_update">>, ?AUTH_DB_DOC_VALIDATE_FUNCTION}
    ],
    {ok, couch_doc:from_json_obj({DocProps})}.
