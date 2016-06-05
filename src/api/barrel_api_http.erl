%% Copyright 2016, Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://joinup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

-module(barrel_api_http).

-export([get_listeners/1]).
-export([binding_spec/3]).

%% NOTE: until the old mochiweb interface is enabled start on the port 5985.
-define(DEFAULT_BINDING, {"127.0.0.1", 5985}).

-define(DEFAULT_MAX_CONNECTIONS, 10000).
-define(DEFAULT_NB_ACCEPTORS, 10).
-define(DEFAULT_BACKLOG, 2048).
-define(DEFAULT_NODELAY, true).

-spec get_listeners(list()) -> list().
get_listeners(Config) ->
  get_listeners(Config, http) ++ get_listeners(Config, https).

get_listeners(Config, Scheme) ->
  Listeners = proplists:get_value(Scheme, Config, []),
  lists:usort([{Scheme, Binding} || Binding <- Listeners]).


-spec binding_spec(list(), atom(), list()) -> tuple().
binding_spec(Config, Scheme, Binding) ->
  {Addr, Port} = Binding,
  Ip = parse_address(Addr),
  Ref = spec_name(Scheme, Ip, Port),
  NbAcceptors = proplists:get_value(nb_acceptors, Config, ?DEFAULT_NB_ACCEPTORS),
  TransportOpts = transport_opts(Scheme, Ip, Port, Config),
  Transport = scheme_to_transport(Scheme),
  ProtoOpts = protocol_opts(),
  ranch:child_spec(Ref, NbAcceptors, Transport, TransportOpts, cowboy_protocol, ProtoOpts).


%% @doc convenient function to parse an address
-spec parse_address(AddrIn) -> AddrOut when
      AddrIn :: inet:ip_address() | inet:hostname(),
      AddrOut :: inet:ip_address().
parse_address({_, _, _, _}=Addr) -> Addr;
parse_address({_, _, _, _, _, _, _, _}= Addr) -> Addr;
parse_address(S) ->
    {ok, Addr} = inet:parse_address(S),
    Addr.

spec_name(Scheme, Ip, Port) ->
  FormattedIP = if is_tuple(Ip); tuple_size(Ip) == 4 ->
                     inet_parse:ntoa(Ip);
                   is_tuple(Ip); tuple_size(Ip) == 8 ->
                     [$[, inet_parse:ntoa(Ip), $]];
                   true -> Ip
                end,
  list_to_atom(lists:flatten(io_lib:format("~s://~s:~p", [Scheme, FormattedIP, Port]))).


transport_opts(http, Ip, Port, Config) ->
  common_opts(Ip, Port, Config);
transport_opts(https, Ip, Port, Config) ->
  lists:flatten(common_opts(Ip, Port, Config), barrel_ssl:options(Config)).

scheme_to_transport(http) -> ranch_tcp;
scheme_to_transport(https) -> ranch_ssl.

common_opts(Ip, Port, Config) ->
  Backlog = proplists:get_value(backlog, Config, ?DEFAULT_BACKLOG),
  Nodelay = proplists:get_value(nodelay, Config, ?DEFAULT_NODELAY),
  MaxConn = proplists:get_value(max_connections, Config, ?DEFAULT_MAX_CONNECTIONS),
  [{max_connections, MaxConn}, {ip, Ip}, {port, Port}, {backlog, Backlog}, {nodelay, Nodelay}].

protocol_opts() ->
  Dispatch = cowboy_router:compile([
                                    {'_', [{"/", barrel_http_root, []}]}
                                   ]),
  [{env, [{dispatch, Dispatch}]}].
