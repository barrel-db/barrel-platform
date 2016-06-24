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

%% @doc a module to keep our secret token

-module(barrel_auth).
-author("Benoit Chesneau").
-behaviour(gen_server).

%% API
-export([secret/0, secret/1]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").
-include("barrel.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #{}.

%%%===================================================================
%%% API
%%%===================================================================

secret() ->
  case ?catch_val(auth_secret) of
    {'EXIT', _} ->
      case application:get_env(barrel, auth_secret) of
        {ok, S} -> S;
        _ ->
          S = gen_server:call(barrel_auth, get_secret),
          barrel_lib:set(auth_secret, S),
          S
      end;
    S -> S
  end.

secret(Secret) -> get_server:call(barrel_auth, {set_secret, node(), Secret}).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, barrel_auth}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init([]) ->
  process_flag(trap_exit, true),
  {ok, init_secret()}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(get_secret, _From, #{secret := S}=State) ->
  {reply, S, State};
handle_call({set_secret, Node, Secret}, _From, State) when Node =:= node() ->
  barrel_lib:set(auth_secret, Secret),
  {reply, ok, State#{secret => Secret}}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
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

init_secret() ->
  case init:get_argument(authsecret) of
    {ok, [[S0]]} ->
      S = list_to_binary(S0),
      #{secret => S};
    _ ->
      case read_secret() of
        {error, Error} ->
          error_logger:error_msg(Error, []),
          %% Is this really this serious?
          erlang:error(Error);
        {ok, S} ->
          #{secret => S}
      end
  end.

read_secret() ->
  case init:get_argument(home) of
    {ok, [[Home]]} ->
      read_secret(filename:join(Home, ".barrel.cookie"));
    _ ->
      {error, "No home for barrel cookie file"}
  end.

read_secret(Name) ->
  case file:raw_read_file_info(Name) of
    {ok, #file_info {type=Type, mode=Mode, size=Size}} ->
      case check_attributes(Name, Type, Mode, os:type()) of
        ok -> read_secret(Name, Size);
        Error -> Error
      end;
    {error, enoent} ->
      case create_secret(Name) of
        ok -> read_secret(Name);
        Error -> Error
      end;
    {error, Reason} ->
      {error, make_error(Name, Reason)}
  end.

read_secret(Name, Size) ->
  case file:open(Name, [raw, read, binary]) of
    {ok, File} ->
      case file:read(File, Size) of
        {ok, Bin} ->
          ok = file:close(File),
          check_secret(Bin);
        {error, Reason} ->
          make_error(Name, Reason)
      end;
    {error, Reason} ->
      make_error(Name, Reason)
  end.

check_secret(<<>>) -> {error, "too short secret string"};
check_secret(Bin) ->
  case catch barrel_lib:hex_to_binary(Bin) of
    {'EXIT', _} -> {error, "Bad characters in secret"};
    S -> {ok, S}
  end.

create_secret(Name) ->
  Secret = barrel_lib:to_hex(crypto:rand_bytes(20)),
  case file:open(Name, [write, raw]) of
    {ok, File} ->
      R1 = file:write(File, Secret),
      ok = file:close(File),
      R2 = file:raw_write_file_info(Name, make_info(Name)),
      case {R1, R2} of
        {ok, ok} ->
          ok;
        {{error,Reason}, _} ->
          {error,
            lists:flatten(
              io_lib:format("Failed to write to secret file '~ts': ~p", [Name, Reason]))};
        {ok, {error, Reason}} ->
          {error, "Failed to change mode: " ++ atom_to_list(Reason)}
      end;
    {error,Reason} ->
      {error,
        lists:flatten(
          io_lib:format("Failed to create secret file '~ts': ~p", [Name, Reason]))}
  end.


make_error(Name, Reason) ->
  {error, "Error when reading " ++ Name ++ ": " ++ atom_to_list(Reason)}.

make_info(Name) ->
  Midnight =
    case file:raw_read_file_info(Name) of
      {ok, #file_info{atime={Date, _}}} ->
        {Date, {0, 0, 0}};
      _ ->
        {{1990, 1, 1}, {0, 0, 0}}
    end,
  #file_info{mode=8#400, atime=Midnight, mtime=Midnight, ctime=Midnight}.

check_attributes(Name, Type, _Mode, _Os) when Type =/= regular ->
  {error, "Secret file " ++ Name ++ " is of type " ++ Type};
check_attributes(Name, _Type, Mode, {unix, _}) when (Mode band 8#077) =/= 0 ->
  {error, "Secret file " ++ Name ++ " must be accessible by owner only"};
check_attributes(_Name, _Type, _Mode, _Os) ->
  ok.
