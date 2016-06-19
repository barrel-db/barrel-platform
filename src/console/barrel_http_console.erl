
%% Copyright (c) 2016, Benoit Chesneau
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
%%

%% Created by benoitc on 10/06/16.

-module(barrel_http_console).
-author("Benoit Chesneau").

%% API
-export([childspec/1]).
-export([config/0]).
-export([is_enabled/0]).
-export([binding/1]).
-export([admin_uri/0]).

-include("barrel.hrl").

-define(DEFAULT_ADDRESS, "127.0.0.1").
-define(DEFAULT_PORT, 5985).
-define(DEFAULT_NB_ACCEPTORS, 10).
-define(DEFAULT_SCHEME, http).


childspec(Config) ->
  {Scheme, Addr, Port} = binding(Config),
  Ip = barrel_api_http:parse_address(Addr),
  NbAcceptors = proplists:get_value(nb_acceptors, Config, ?DEFAULT_NB_ACCEPTORS),
  Transport = barrel_api_http:scheme_to_transport(Scheme),
  ProtoOpts = protocol_opts(),
  TransportOpts = barrel_api_http:transport_opts(Scheme, Ip, Port, Config),
  ranch:child_spec(spec_name(Scheme), NbAcceptors, Transport, TransportOpts,
    cowboy_protocol, ProtoOpts).


config() ->
  case ?catch_val(console) of
    {'EXIT', _} ->
      Config = [{scheme, http}, {port, ?DEFAULT_PORT}, {address, ?DEFAULT_ADDRESS}],
      barrel_lib:set(console, Config),
      Config;
    Config ->
      Config
  end.


is_enabled() -> barrel_server:get_env(start_console).

binding(Config) ->
  Port = proplists:get_value(port, Config, ?DEFAULT_PORT),
  if
    is_integer(Port) -> ok;
    true -> erlang:error({console, bad_port})
  end,

  Addr =  proplists:get_value(address, Config, ?DEFAULT_ADDRESS),
  Scheme = case proplists:get_value(scheme, Config, ?DEFAULT_SCHEME) of
             http -> http;
             https -> https;
             _ -> erlang:error({console, bad_scheme})
           end,
  {Scheme, Addr, Port}.

spec_name(Scheme) ->
  lists:flatten(io_lib:format("~s://console", [Scheme])).

admin_uri() ->
  Config = config(),
  Scheme = proplists:get_value(scheme, Config, ?DEFAULT_SCHEME),
  {Addr, Port} = ranch:get_addr(spec_name(Scheme)),

  lists:flatten([atom_to_list(Scheme), "://", inet:ntoa(Addr), ":", integer_to_list(Port)]).

protocol_opts() ->
  Dispatch = cowboy_router:compile([
    {'_', routes()}
  ]),
  [{env, [{dispatch, Dispatch}]}, {middlewares, [barrel_auth_middleware, cowboy_router,
    cowboy_handler]}].

routes() ->
 lists:reverse(
   [{"/[...]", cowboy_static, {priv_dir, barrel, "www"}} |
    prefix_routes(barrel_api_http:routes(), "/dbs",
      [{"/", cowboy_static, {priv_file, barrel, "www/index.html"}}])]).

prefix_routes([{'_', Handler, HandlerOpts} | Rest], Prefix, Acc) ->
  Path = lists:flatten(Prefix ++ "/[...]"),
  prefix_routes(Rest,Prefix,  [{Path, Handler, [{prefix, Prefix} |HandlerOpts]} | Acc]);
prefix_routes([{Path, Handler, HandlerOpts} | Rest], Prefix, Acc) ->
  NewPath = lists:flatten(Prefix ++ Path),
  prefix_routes(Rest, Prefix, [{NewPath, Handler, [{prefix, Prefix} |HandlerOpts]} | Acc]);
prefix_routes([], _Prefix, Acc) ->
  Acc.
