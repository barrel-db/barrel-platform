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

-module(barrel_auth_cache).
-behaviour(gen_server).

% public API
-export([get_user_creds/1]).
-export([has_admins/0]).
-export([start_link/0]).

% gen_server API
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).

-include("couch_db.hrl").
-include("couch_js_functions.hrl").

-define(STATE, auth_state_ets).
-define(BY_USER, auth_by_user_ets).
-define(BY_ATIME, auth_by_atime_ets).


-define(DEFAULT_USERDB, <<"_users">>).
-define(DEFAULT_CACHE_SIZE, 50).
-define(DNAME, <<"_design/_auth" >>).

-record(state, {
    max_cache_size = 0,
    cache_size = 0,
    db_notifier = nil,
    db_mon_ref = nil
}).


-spec get_user_creds(UserName::string() | binary()) ->
    Credentials::list() | nil.

get_user_creds(UserName) ->
    UserCreds = get_from_cache(barrel_lib:to_binary(UserName)),
    validate_user_creds(UserCreds).

has_admins() ->
  exec_if_auth_db(
    fun(Db) ->
      Args = [{start_key, <<"_admin">>}, {end_key, <<"_admin">>}, {limit,1}],
      {ok, Acc} = couch_mrview:query_view(Db, ?DNAME,
        <<"by_roles">>, Args, fun view_cb/2, nil),
      Acc /= []
    end).


view_cb({row, Row}, _Acc) ->
  Val = proplists:get_value(value, Row),
  {ok, Val};
view_cb(_Other, Acc) ->
  {ok, Acc}.


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
    case maps:get(<<"_conflicts">>, UserCreds, undefined) of
    undefined ->
        ok;
    _ConflictList ->
        throw({unauthorized,
            <<"User document conflicts must be resolved before the document",
              " is used for authentication purposes.">>
        })
    end,
    UserCreds.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    ?STATE = ets:new(?STATE, [set, protected, named_table]),
    ?BY_USER = ets:new(?BY_USER, [set, protected, named_table]),
    ?BY_ATIME = ets:new(?BY_ATIME, [ordered_set, private, named_table]),
    process_flag(trap_exit, true),
    CacheSize = barrel_server:get_env(auth_cache_size),
    _ = barrel_event:reg(<<"_users">>),
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

handle_info({'$barrel_event', _, Event}, State) ->
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
    catch barrel_event:unreg(),
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
    State#state{cache_size = proplists:get_value(size, ets:info(?BY_USER))}.

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
    {Creds} = barrel_doc:to_json_obj(Doc, []),
    Creds.


is_user_doc(#doc_info{id = <<"org.barrel.user:", UserName/binary>>}) ->
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
    {ok, AuthDb} = ensure_users_db_exists([sys_db]),
    AuthDb.


get_user_props_from_db(UserName) ->
    exec_if_auth_db(
        fun(AuthDb) ->
            Db = reopen_auth_db(AuthDb),
            DocId = <<"org.barrel.user:", UserName/binary>>,
            try
                {ok, Doc} = couch_db:open_doc(Db, DocId, [conflicts]),
                DocProps = barrel_doc:to_json_obj(Doc, []),
                DocProps
            catch
            _:_Error ->
                nil
            end
        end,
        nil
    ).

ensure_users_db_exists(Options) ->
    Options1 = [{user_ctx, barrel_lib:adminctx()}, nologifmissing | Options],
    case couch_db:open(<<"_users">>, Options1) of
    {ok, Db} ->
        setup_db(Db),
        {ok, Db};
    _Error ->
        {ok, Db} = couch_db:create(<<"_users">>, Options1),
        ok = setup_db(Db),
        {ok, Db}
    end.

setup_db(Db) ->
  UserDoc = case couch_db:open_doc(Db, <<"_design/_auth">>) of
              {not_found, _} ->
                [auth_design_doc()];
              {ok, D} ->
                Obj = barrel_doc:to_json_obj(D, []),
                ValidFun = maps:get(<<"validate_doc_update">>, Obj, undefined),
                Views = maps:get(<<"views">>, Obj, #{}),
                ByRolesFun = maps:get(<<"by_roles">>, Views, undefined),

                Reset = ValidFun /= ?AUTH_DB_DOC_VALIDATE_FUNCTION
                  orelse ByRolesFun /= ?AUTH_VIEW_BY_ROLES,

                case Reset of
                  false -> [];
                  true ->
                    Views1 = Views#{<<"by_roles">> => ?AUTH_VIEW_BY_ROLES},
                    Obj1 = Obj#{ <<"validate_doc_update">> => ?AUTH_DB_DOC_VALIDATE_FUNCTION,
                                 <<"views">> => Views1},
                    [barrel_doc:from_json_obj(Obj1)]
                end
            end,
  AdminDoc = case couch_db:open_doc(Db, <<"org.barrel.user:barrel">>) of
               {not_found, _} ->
                 [admin_doc()];
               {ok, _D} ->
                 []
             end,
  {ok, _} = couch_db:update_docs(Db, UserDoc ++ AdminDoc),
  ok.

auth_design_doc() ->
    Obj = #{  <<"_id">> => <<"_design/_auth">>,
              <<"language">> => <<"javascript">>,
              <<"validate_doc_update">> => ?AUTH_DB_DOC_VALIDATE_FUNCTION,
              <<"views">> => #{ <<"by_roles">> => ?AUTH_VIEW_BY_ROLES }
           },
    barrel_doc:from_json_obj(Obj).

admin_doc() ->
  Obj = #{ <<"_id">> => <<"org.barrel.user:barrel">>,
           <<"name">> => <<"barrel">>,
           <<"roles">> => [<<"_admin">>],
           <<"password">> => <<"admin">>,
           <<"type">> => <<"user">>
         },
  barrel_doc:from_json_obj(Obj).
