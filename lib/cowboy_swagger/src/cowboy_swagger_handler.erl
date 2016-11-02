%%% @doc Cowboy Swagger Handler. This handler exposes a GET operation
%%%      to enable that  `swagger.json' can be retrieved from embedded
%%%      Swagger-UI (located in `priv/swagger' folder).
-module(cowboy_swagger_handler).

%% Trails
-behaviour(trails_handler).
-export([trails/0, trails/1]).

-type route_match() :: '_' | iodata().
-type options() :: #{server => ranch:ref(), host => route_match()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trails
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
%% @doc Implements `trails_handler:trails/0' callback. This function returns
%%      trails routes for both: static content (Swagger-UI) and this handler
%%      that returns the `swagger.json'.
-spec trails() -> trails:trails().
trails() -> trails(#{}).
-spec trails(Options::options()) -> trails:trails().
trails(Options) ->
  StaticFiles =
    case application:get_env(cowboy_swagger, static_files) of
      {ok, Val} -> Val;
      _         -> filename:join(cowboy_swagger_priv(), "swagger")
    end,
  Redirect = trails:trail(
    "/api-docs",
    cowboy_swagger_redirect_handler,
    {file, StaticFiles ++ "/index.html"},
    #{get => #{hidden => true}}),
  Static = trails:trail(
    "/api-docs/[...]",
    cowboy_static,
    {dir, StaticFiles, [{mimetypes, cow_mimetypes, all}]},
    #{get => #{hidden => true}}),
  MD = #{get => #{hidden => true}},
  Handler = trails:trail(
    "/api-docs/swagger.json", cowboy_swagger_json_handler, Options, MD),
  [Redirect, Handler, Static].

%% @private
-spec cowboy_swagger_priv() -> string().
cowboy_swagger_priv() ->
  case code:priv_dir(cowboy_swagger) of
    {error, bad_name} ->
      case code:which(cowboy_swagger_handler) of
        cover_compiled -> "../../priv"; % required for tests to work
        BeamPath -> filename:join([filename:dirname(BeamPath) , ".." , "priv"])
      end;
    Path -> Path
  end.
