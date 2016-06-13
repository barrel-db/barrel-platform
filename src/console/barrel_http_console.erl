
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
-export([is_enabled/0]).
-export([binding/1]).
-export([admin_uri/0]).

-define(DEFAULT_ADDRESS, "127.0.0.1").
-define(DEFAULT_PORT, 5985).
-define(DEFAULT_NB_ACCEPTORS, 10).
-define(DEFAULT_SCHEME, http).


childspec(Config) ->
  {Scheme, Addr, Port} = binding(Config),
  Ip = barrel_api_http:parse_address(Addr),
  NbAcceptors = barrel_config:pget_int(nb_acceptors, Config, ?DEFAULT_NB_ACCEPTORS),
  Transport = barrel_api_http:scheme_to_transport(Scheme),
  ProtoOpts = protocol_opts(),
  TransportOpts = barrel_api_http:transport_opts(Scheme, Ip, Port, Config),
  ranch:child_spec(barrel_console, NbAcceptors, Transport, TransportOpts,
    cowboy_protocol, ProtoOpts).


is_enabled() -> barrel_config:get_boolean("console", "enabled", false) =:= true.

binding(Config) ->
  Port = barrel_config:pget_int(port, Config, ?DEFAULT_PORT),
  Addr =  proplists:get_value(address, Config, ?DEFAULT_ADDRESS),

  Scheme = case proplists:get_value(scheme, Config, ?DEFAULT_SCHEME) of
             "http" -> http;
             "https" -> https;
             http -> http;
             https -> https;
             _ -> erlang:error(bad_scheme)
           end,
  {Scheme, Addr, Port}.

admin_uri() ->
  ConsoleCfg = barrel_config:section_to_opts("console"),
  {Scheme, Addr, Port} = binding(ConsoleCfg),
  lists:flatten([atom_to_list(Scheme), "://", Addr, ":", integer_to_list(Port)]).

protocol_opts() ->
  io:format("routes are ~p~n", [routes()]),
  Dispatch = cowboy_router:compile([
    {'_', routes()}
  ]),
  [{env, [{dispatch, Dispatch}]}].


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
