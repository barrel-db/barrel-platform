%% Copyright 2016, Benoit Chesneau
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


-module(barrel_api_http).

-export([get_listeners/0]).
-export([binding_spec/2]).
-export([web_uris/0]).

%% http helpers
-export([host/1]).


%% internal apis
-export([routes/0]).
-export([parse_address/1]).
-export([transport_opts/4]).
-export([scheme_to_transport/1]).
-export([cleanup_listener_opts/1]).


%% NOTE: until the old mochiweb interface is enabled start on the port 5985.
-define(DEFAULT_ADDRESS, "127.0.0.1").
-define(DEFAULT_SCHEME, http).

-define(DEFAULT_MAX_CONNECTIONS, 10000).
-define(DEFAULT_NB_ACCEPTORS, 10).
-define(DEFAULT_BACKLOG, 2048).
-define(DEFAULT_NODELAY, true).


-spec get_listeners() -> list().
get_listeners() -> lists:usort(barrel_server:get_env(listen)).


-spec binding_spec(atom(), list()) -> tuple().
binding_spec(Ref, Opts) ->
  {Scheme, Ip, Port} = binding(Opts),
  NbAcceptors = proplists:get_value(nb_acceptors, Opts, ?DEFAULT_NB_ACCEPTORS),
  TransportOpts = transport_opts(Scheme, Ip, Port, Opts),
  Transport = scheme_to_transport(Scheme),
  ProtoOpts = protocol_opts(),
  ranch:child_spec(Ref, NbAcceptors, Transport, TransportOpts, cowboy_protocol, ProtoOpts).

web_uris() ->
  Acc = lists:foldl(fun({Ref, Opts}, Acc1) ->
      {Scheme, _Ip, _Port} = binding(Opts),
      {LAddr, LPort} = ranch:get_addr(Ref),
      URI = lists:flatten([atom_to_list(Scheme), "://", inet:ntoa(LAddr), ":", integer_to_list(LPort)]),
      [list_to_binary(URI) | Acc1]
    end, [], barrel_lib:val(listen, [])),
  lists:usort(Acc).

%% @doc convenient function to parse an address
-spec parse_address(AddrIn) -> AddrOut when
      AddrIn :: inet:ip_address() | inet:hostname(),
      AddrOut :: inet:ip_address().
parse_address({_, _, _, _}=Addr) -> Addr;
parse_address({_, _, _, _, _, _, _, _}= Addr) -> Addr;
parse_address(S) ->
    {ok, Addr} = inet:parse_address(S),
    Addr.

transport_opts(http, Ip, Port, Opts) ->
  common_opts(Ip, Port, Opts);
transport_opts(https, Ip, Port, Opts) ->
  lists:flatten(common_opts(Ip, Port, Opts), barrel_ssl:options(Opts)).

scheme_to_transport(http) -> ranch_tcp;
scheme_to_transport(https) -> ranch_ssl.

common_opts(Ip, Port, Opts) ->
  Backlog = proplists:get_value(backlog, Opts, ?DEFAULT_BACKLOG),
  Nodelay = proplists:get_value(nodelay, Opts, ?DEFAULT_NODELAY),
  MaxConn = proplists:get_value(max_connections, Opts, ?DEFAULT_MAX_CONNECTIONS),
  [{max_connections, MaxConn}, {ip, Ip}, {port, Port}, {backlog, Backlog}, {nodelay, Nodelay}].

protocol_opts() ->
  Dispatch = cowboy_router:compile([
                                    {'_', routes()}
                                   ]),
  [{env, [{dispatch, Dispatch}]}, {middlewares, [barrel_cors_middleware, barrel_auth_middleware,
    cowboy_router, cowboy_handler]}].

binding(Opts) ->
  Port = proplists:get_value(port, Opts, 0),
  Addr = proplists:get_value(address, Opts, ?DEFAULT_ADDRESS),
  Ip = parse_address(Addr),
  Scheme = proplists:get_value(scheme, Opts, ?DEFAULT_SCHEME),
  {Scheme, Ip, Port}.

routes() ->
  [
    {"/", barrel_root_handler, []},
    {"/_session", barrel_session_handler, []},
    {'_', barrel_legacy_handler, barrel_legacy_handler:options()}
  ].


host(Req) ->
  XHost = barrel_server:get_env(x_forwarded_host),
  case cowboy_req:header(XHost, Req) of
    {undefined, _Req} ->
      case cowboy_req:header(<<"host">>, Req) of
        {undefined, _} -> <<>>;
        {Host, _} -> Host
      end;
    {Host, _} -> Host
  end.

cleanup_listener_opts(Ref) ->
  Listeners = proplists:delete(Ref, barrel_lib:val(listeners, [])),
  barrel_lib:set(listeners, Listeners).
