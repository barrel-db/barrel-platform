%% Copyright (c) 2016. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Created by benoitc on 20/06/16.

-module(barrel_users_local).
-author("Benoit Chesneau").
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API

-export([is_admin/1]).
-export([has_admins/0]).
-export([get_user/1]).
-export([set_user/4]).
-export([delete_user/1]).

-export([dump/0]).
-export([reload/0]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-define(LOCAL_USERS, barrel_local_users).
-define(LOCAL_USERS_ROLES, barrel_local_roles).

-define(LOCAL_DISC_USERS, barrel_local_users_disc).

-record(state, {name}).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc check if this user an admin
is_admin(#{ roles := Roles}) ->
  lists:member(<<"_admin">>, Roles);
is_admin(User) when is_binary(User) ->
  case ets:lookup(?LOCAL_USERS, User) of
    [] -> false;
    [{User, #{roles := Roles}}] -> lists:member(<<"_admin">>, Roles)
  end;
is_admin(_) ->
  erlang:error(badarg).

%% @doc check if the database has admin
has_admins() ->
  ets:select_count(?LOCAL_USERS_ROLES, ets:fun2ms(fun({R, _}) when R =:= <<"_admin">> -> true end)) > 0.

%% @doc get a local user by its username
-spec get_user(binary()) -> map().
get_user(User) ->
  case ets:lookup(?LOCAL_USERS, User) of
    [] -> {error, not_found};
    [{User, Doc}] -> {ok, Doc}
  end.

%% @doc update a local user
-spec set_user(binary(), binary(), list(), map()) -> ok | {error, any()}.
set_user(User, Password, Roles, Meta)
    when is_binary(User), is_binary(Password), is_list(Roles), is_map(Meta) ->
  gen_server:call(?MODULE, {set_user, User, Password, Roles, Meta}).

%% @doc delete a local user
-spec delete_user(binary()) -> ok | {error, any()}.
delete_user(User) when is_binary(User) ->
  gen_server:call(?MODULE, {delete_user, User}).

%% @doc dump the memory to disk
-spec dump() -> ok.
dump() ->
  gen_server:cast(?MODULE, dump).

%% @doc reload the passwords from disk
-spec reload() -> ok.
reload() ->
  gen_server:call(?MODULE, reload).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init([]) ->
  ets:new(?LOCAL_USERS, [ordered_set, named_table, protected,
    {read_concurrency, true}]),
  ets:new(?LOCAL_USERS_ROLES, [bag, named_table, protected,
    {read_concurrency, true}]),
  {ok, init_local_users()}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.

handle_call(dump, _From, State) ->
  case update_file(State#state.name) of
    {ok, NewName} -> {reply, ok, State#state{name=NewName}};
    Error -> {reply, Error, State}
  end;

handle_call({set_user, User, Password, Roles, Meta}, _From, State) ->
  {Reply, NewState2} = case do_set_user( User, Password, Roles, Meta, State) of
                        {ok, NewState} -> {ok, NewState};
                        Error -> {Error, State}
                      end,
  {reply, Reply, NewState2};

handle_call({delete_user, User}, _From, State) ->
  {Reply, NewState2} = case do_delete_user( User, State) of
                         {ok, NewState} -> {ok, NewState};
                         Error -> {Error, State}
                       end,
  {reply, Reply, NewState2};
handle_call(reload, _From, State) ->
  {reply, ok, reload(State)}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(dump, State) ->
  case update_file(State#state.name) of
    {ok, NewName} -> {noreply, State#state{name=NewName}};
    _Error -> {noreply, State}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


passwd_file() ->
  DataDir = barrel_server:get_env(dir),
  case filelib:wildcard(filename:join(DataDir, "PASSWD.*")) of
    [] -> filename:join(DataDir, "PASSWD.0");
    Files ->
      [Last |_] = lists:reverse(lists:sort(Files)),
      Last
  end.

init_local_users() ->
  Name = passwd_file(),
  case read_file(Name) of
    ok ->
      case erase(passwd_updated) of
        true ->
          case update_file(Name) of
            {ok, NewName} -> #state{name = NewName};
            _Error -> #state{name = Name}
          end;
        false ->
          #state{name = Name}
      end;
    Error ->
      error_logger:error_msg(Error, []),
      erlang:error(Error)
  end.

reload(State) ->
  DataDir = barrel_server:get_env(dir),
  Backup = filename:join(DataDir, ".PASSWD.dbswp"),
  case filelib:is_file(Backup) of
    true -> file:delete(Backup);
    _ -> ok
  end,
  dets:open_file(?LOCAL_DISC_USERS, [{file, Backup}]),
  case dets:from_ets(?LOCAL_DISC_USERS, ?LOCAL_USERS) of
    ok ->
      try
        reload1(State)
      after
        (catch dets:close(?LOCAL_DISC_USERS)),
        file:delete(Backup)
      end;
    Error ->
      Error
  end.

reload1(State) ->
  ets:delete_all_objects(?LOCAL_USERS),
  ets:delete_all_objects(?LOCAL_USERS_ROLES),
  case read_file(State#state.name) of
    ok ->
      case erase(passwd_updated) of
        true ->
          case update_file(State#state.name) of
            {ok, NewName} -> #state{name = NewName};
            _Error -> State
          end;
        false ->
          State
      end;
    Error ->
      case dets:to_ets(?LOCAL_DISC_USERS, ?LOCAL_USERS) of
        ok ->
          ets:foldl(fun({User, #{roles := Roles}}, Acc1) ->
                      [ets:insert(?LOCAL_USERS_ROLES, {User, Role}) || Role <- Roles],
                      Acc1
                    end, ok, ?LOCAL_USERS),
          State;
        Error ->
          (catch dets:close(?LOCAL_DISC_USERS)),
          error_logger:error_msg(Error, []),
          erlang:error(Error)
      end
  end.



read_file(Name) ->
  case file:raw_read_file_info(Name) of
    {ok, #file_info {type=Type, mode=Mode, size=Size}} ->
      case check_attributes(Name, Type, Mode, os:type()) of
        ok -> read_file1(Name, Size);
        Error -> Error
      end;
    {error, enoent} ->
      case create_file(Name) of
        ok -> read_file(Name);
        Error -> Error
      end;
    {error, Reason} ->
      {error, "failed to read the password file: " ++ atom_to_list(Reason)}
  end.

read_file1(Name, Size) ->
  case file:open(Name, [raw, read, binary]) of
    {ok, File} ->
      put(passwd_updated, false),
      case file:read(File, Size) of
        {ok, Bin} ->
          ok = file:close(File),
          Lines = re:split(Bin, "\r\n|\n|\r|\032", [{return, binary}]),
          process_file(Lines);
        Error ->
          Error
      end;
    Error ->
      Error
  end.


check_attributes(Name, Type, _Mode, _Os) when Type =/= regular ->
  {error, "Secret file " ++ Name ++ " is of type " ++ Type};
check_attributes(Name, _Type, Mode, {unix, _}) when (Mode band 8#077) =/= 0 ->
  {error, "Secret file " ++ Name ++ " must be accessible by owner only"};
check_attributes(_Name, _Type, _Mode, _Os) ->
  ok.


process_file([Line | Rest]) ->
  case catch process_line(Line) of
    ok -> process_file(Rest);
    {error, Error} -> {error, Error}
  end;
process_file([]) ->
  ok.

process_line(<<>>) -> ok;
process_line(<<";", _/binary>>) -> ok; %% comment line
process_line(<<"#", _/binary>>) -> ok; %% comment line
process_line(Line) ->
  case catch binary:split(Line, <<":">>, [global]) of
    [] -> {error, "error when reading the password file"};
    [Line] -> {error, "Failed to process the password file"};
    [Name, <<>>] ->
      ets:insert(?LOCAL_USERS, {Name, null_doc()}),
      ok;
    [Name, PasswordBin] ->
      insert_user(Name, parse_password(PasswordBin), [<<"_admin">>], #{});
    [Name, PasswordBin, RolesBin] ->
      insert_user(Name, parse_password(PasswordBin), parse_roles(RolesBin), #{});
    [Name, PasswordBin, RolesBin, MetaBin|_] ->
      insert_user(Name, parse_password(PasswordBin), parse_roles(RolesBin),
        parse_meta(MetaBin));
    Error ->
      Error
  end.

insert_user(Name, [DerivedKey, Salt, Iterations], Roles, Meta) ->
  Doc = user_doc(DerivedKey, Salt, Iterations, Roles, Meta),
  true = ets:insert(?LOCAL_USERS, {Name, Doc}),
  lists:foreach(fun(Role) ->
      true = ets:insert(?LOCAL_USERS_ROLES, {Role, Name})
    end, Roles),
  ok.

parse_password(<<"{pbkdf2}", Bin/binary>>) ->
  case catch binary:split(Bin, <<",">>, [global]) of
    [DerivedKey, Salt, Iterations] ->
      [DerivedKey, Salt, binary_to_integer(Iterations)];
    _ -> throw({error, "Failed to process file. Invalid password."})
  end;
parse_password( <<"{", _/binary>>) -> throw({error, "Failed to process the password file. Invalid password."}) ;
parse_password(ClearPassword) ->
  put(passwd_updated, true),
  Iterations = barrel_server:get_env(auth_pbkdf2_iterations),
  Salt = barrel_uuids:random(),
  DerivedKey = couch_passwords:pbkdf2(ClearPassword, Salt, Iterations),
  [DerivedKey, Salt, Iterations].

parse_roles(RolesBin) ->
  (catch binary:split(RolesBin, <<",">>, [global])).

parse_meta(MetaBin) ->
  parse_meta(binary:split(MetaBin, <<" ">>, [global]), #{}).

parse_meta([<<>>], Acc) -> Acc;
parse_meta([KV | Rest], Acc) ->
  case binary:split(KV, <<"=">>) of
    [K, V] -> parse_meta(Rest, Acc#{K => V});
    _ -> throw({error, "Failed to process the password file. Invalid meta"})
  end;
parse_meta([], Acc) ->
  Acc.

create_file(Name) ->
  case file:open(Name, [write, raw]) of
    {ok, File} ->
      R1 = file:write(File, <<"barrel:">>),
      ok = file:close(File),
      R2 = file:raw_write_file_info(Name, make_info(Name)),
      case {R1, R2} of
        {ok, ok} ->
          ok;
        {{error,Reason}, _} ->
          {error,
            lists:flatten(
              io_lib:format("Failed to write to password file '~ts': ~p", [Name, Reason]))};
        {ok, {error, Reason}} ->
          {error, "Failed to change mode: " ++ atom_to_list(Reason)}
      end;
    {error,Reason} ->
      {error,
        lists:flatten(
          io_lib:format("Failed to create password file '~ts': ~p", [Name, Reason]))}
  end.

update_file(Name) ->
  Lines = ets:foldr(fun({User, Doc}, Acc1) ->
                      case Doc of
                        #{<<"password_scheme">> := <<"pbkdf2">> } ->
                          #{<<"roles">> := Roles,
                            <<"salt">> := Salt,
                            <<"iterations">> := Iterations,
                            <<"derived_key">> := DerivedKey,
                            <<"meta">> := Meta} = Doc,

                          Meta1 = maps:fold(fun(Key, Val, Acc) ->
                            [<< Key/binary, "=", Val/binary >> | Acc]
                                            end, [], Meta),

                          Line = [User, ":", "{pbkdf2}", DerivedKey, ",", Salt, ",",
                            integer_to_list(Iterations), ":", barrel_lib:join(Roles, <<",">>), ":",
                            barrel_lib:join(lists:reverse(Meta1), <<",">>), "\n"],
                          [Line | Acc1];
                        _ ->
                          [io_lib:format("~s:~n", [User]) | Acc1]
                      end
                    end, [], ?LOCAL_USERS),
  NewName = inc_file(Name),
  case file:open(NewName, [write, raw]) of
    {ok, File} ->
      R1 = file:write(File, Lines),
      ok = file:close(File),
      R2 = file:raw_write_file_info(NewName, make_info(NewName)),
      case {R1, R2} of
        {ok, ok} ->
          barrel_lib:delete_file(Name, true),
          {ok, NewName};
        {{error,Reason}, _} ->
          barrel_lib:delete_file(NewName, true),
          {error,
            lists:flatten(
              io_lib:format("Failed to update the password file '~ts': ~p", [NewName, Reason]))};
        {ok, {error, Reason}} ->
          barrel_lib:delete_file(NewName, true),
          {error, "Failed to change mode: " ++ atom_to_list(Reason)}
      end;

    Error ->
      error_logger:error_msg("Error updating ~p: ~p~n", [Name, Error]),
      {error, Error}
  end.

inc_file(Name) ->
  [S] = string:tokens(filename:extension(Name), "."),
  I = list_to_integer(S) +1,
  Root = barrel_server:get_env(dir),
  filename:join(Root, lists:flatten(["PASSWD", ".", integer_to_list(I)])).


make_info(Name) ->
  Midnight =
    case file:raw_read_file_info(Name) of
      {ok, #file_info{atime={Date, _}}} ->
        {Date, {0, 0, 0}};
      _ ->
        {{1990, 1, 1}, {0, 0, 0}}
    end,
  #file_info{mode=8#00700, atime=Midnight, mtime=Midnight, ctime=Midnight}.

null_doc() ->
  #{<<"roles">> => [<<"_admin">>]}.

user_doc(DerivedKey, Salt, Iterations, Roles, Meta) ->
  #{<<"roles">> => Roles,
    <<"salt">> => Salt,
    <<"iterations">> => Iterations,
    <<"password_scheme">> => <<"pbkdf2">>,
    <<"derived_key">> => DerivedKey,
    <<"meta">> => Meta}.

do_set_user(User, Password, Roles, Meta, State) ->
  {OldRoles, Doc} =
    case ets:lookup(?LOCAL_USERS, User) of
      [] -> {[], false};
      [{User,  #{ <<"_roles">> := R}=D}] -> {R, D}
    end,

  Iterations = barrel_server:get_env(auth_pbkdf2_iterations),
  Salt = barrel_uuids:random(),
  DerivedKey = couch_passwords:pbkdf2(Password, Salt, Iterations),

  insert_user(User, [DerivedKey, Salt, Iterations], Roles, Meta),
  case update_file(State#state.name) of
    {ok, NewName} ->
      RolesToDelete = OldRoles -- Roles,
      lists:foreach(fun(Role) ->
                      ets:delete_object(?LOCAL_USERS_ROLES, {Role, User})
                    end, RolesToDelete),
      {ok, State#state{name = NewName}};
    Error ->
      %% revert the changes,if needed
      RolesToDelete = Roles -- OldRoles,
      case Doc of
        false -> ets:delete(?LOCAL_USERS, User);
        _ -> ets:insert(?LOCAL_USERS, {User, Doc})
      end,
      lists:foreach(fun(Role) ->
                      ets:delete_object(?LOCAL_USERS_ROLES, {Role, User})
                    end, RolesToDelete),
      Error
  end.

do_delete_user(User, State) ->
  case ets:lookup(?LOCAL_USERS, User) of
    [] -> {ok, State};
    [{User, Doc}] ->
      #{ <<"roles">> := Roles } = Doc,
      ets:delete(?LOCAL_USERS, User),
      lists:foreach(fun(Role) ->
          ets:delete_object(?LOCAL_USERS_ROLES, {Role, User})
        end, Roles),
      case update_file(State#state.name) of
        {ok, NewName} -> {ok, State#state{name = NewName}};
        Error ->
          %% revert
          ets:insert(?LOCAL_USERS, {User, Doc}),
          lists:foreach(fun(Role) ->
                          ets:insert(?LOCAL_USERS_ROLES, {Role, User})
                        end, Roles),
          Error
      end
  end.
