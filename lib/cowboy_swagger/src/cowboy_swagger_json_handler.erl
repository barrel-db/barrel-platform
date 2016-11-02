%%% @doc Cowboy Swagger Handler. This handler exposes a GET operation
%%%      to enable that  `swagger.json' can be retrieved from embedded
%%%      Swagger-UI (located in `priv/swagger' folder).
-module(cowboy_swagger_json_handler).

%% Cowboy callbacks
-export([ init/3
        , rest_init/2
        , content_types_provided/2
        ]).

%% Handlers
-export([handle_get/2]).

-type state() :: #{}.
-type route_match() :: '_' | iodata().
-type options() :: #{server => ranch:ref(), host => route_match()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), options()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @hidden
-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), options()}.
rest_init(Req, Opts) ->
  {ok, Req, Opts}.

%% @hidden
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[term()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Server = maps:get(server, State, '_'),
  HostMatch = maps:get(host, State, '_'),
  Trails = trails:all(Server, HostMatch),
  {cowboy_swagger:to_json(Trails), Req, State}.
