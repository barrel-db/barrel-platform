%%% @doc Trails main interface.
%%%      Use the functions provided in this module to inteact with `trails'.
-module(trails).

-export([single_host_compile/1]).
-export([compile/1]).
-export([trail/2]).
-export([trail/3]).
-export([trail/4]).
-export([trail/5]).
-export([trails/1]).
-export([path_match/1]).
-export([handler/1]).
-export([options/1]).
-export([metadata/1]).
-export([constraints/1]).
-export([store/1, store/2]).
-export([all/0, all/1, all/2]).
-export([retrieve/1, retrieve/2, retrieve/3]).
-export([api_root/0, api_root/1]).
-export([servers/0]).
-export([host_matches/1]).

%% Trail specification
-opaque trail() ::
  #{ path_match  => cowboy_router:route_match()
   , constraints => cowboy_router:constraints()
   , handler     => module()
   , options     => any()
   , metadata    => metadata(any())
   }.
-export_type([trail/0]).

%% Exported from cowboy_router.erl
-type route_match() :: '_' | iodata().
-type route_path() :: {Path::route_match(), Handler::module(), Opts::any()}
  | {Path::route_match()
    , cowboy_router:constraints()
    , Handler::module()
    , Opts::any()}.
-type route_rule() :: {Host::route_match(), Paths::[route_path()]}
  | {Host::route_match(), cowboy_router:constraints(), Paths::[route_path()]}.
%% End of exported functions

-type trails() :: [trails:trail() | route_path()].
-export_type([trails/0]).

-type method() :: get | put | post | delete | patch | head | options.
-export_type([method/0]).

-type metadata(X) :: #{method() => X}.
-export_type([metadata/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @equiv compile([{'_', Trails}])
-spec single_host_compile(trails()) ->
  cowboy_router:dispatch_rules().
single_host_compile(Trails) ->
  compile([{'_', Trails}]).

%% @doc Compiles the given list of trails routes, also compatible with
%%      `cowboy' routes.
-spec compile([{Host::route_match(), Trails::trails()}]) ->
  cowboy_router:dispatch_rules().
compile([]) -> [];
compile(Routes) ->
  cowboy_router:compile(
    [{Host, to_route_paths(Trails)} || {Host, Trails} <- Routes]).

%% @doc Translates the given trails paths into `cowboy' routes.
-spec to_route_paths([trail()]) -> cowboy_router:routes().
to_route_paths(Paths) ->
  [to_route_path(Path) || Path <- Paths].

%% @doc Translates a trail path into a route rule.
-spec to_route_path(trail()) -> route_rule().
to_route_path(Trail) when is_map(Trail) ->
  ApiRoot = api_root(),
  PathMatch = maps:get(path_match, Trail),
  ModuleHandler = maps:get(handler, Trail),
  Options = maps:get(options, Trail, []),
  Constraints = maps:get(constraints, Trail, []),
  {ApiRoot ++ PathMatch, Constraints, ModuleHandler, Options};
to_route_path(Trail) when is_tuple(Trail) ->
  Trail.

%% @equiv trail(PathMatch, ModuleHandler, [], #{}, [])
-spec trail(route_match(), module()) -> trail().
trail(PathMatch, ModuleHandler) ->
  trail(PathMatch, ModuleHandler, [], #{}, []).

%% @equiv trail(PathMatch, ModuleHandler, Options, #{}, [])
-spec trail(route_match(), module(), any()) -> trail().
trail(PathMatch, ModuleHandler, Options) ->
  trail(PathMatch, ModuleHandler, Options, #{}, []).

%% @equiv trail(PathMatch, ModuleHandler, Options, MetaData, [])
-spec trail(route_match(), module(), any(), map()) -> trail().
trail(PathMatch, ModuleHandler, Options, MetaData) ->
  trail(PathMatch, ModuleHandler, Options, MetaData, []).

%% @doc This function allows you to add additional information to the
%%      `cowboy' handler, such as: resource path, handler module,
%%      options and metadata. Normally used to document handlers.
-spec trail(route_match()
           , module()
           , any()
           , map()
           , cowboy_router:constraints()) -> trail().
trail(PathMatch, ModuleHandler, Options, MetaData, Constraints) ->
  #{ path_match  => PathMatch
   , handler     => ModuleHandler
   , options     => Options
   , metadata    => MetaData
   , constraints => Constraints
   }.

%% @doc Gets the `path_match' from the given `trail'.
-spec path_match(trail()) -> cowboy_router:route_match().
path_match(Trail) ->
  maps:get(path_match, Trail, []).

%% @doc Gets the `handler' from the given `trail'.
-spec handler(trail()) -> module().
handler(Trail) ->
  maps:get(handler, Trail, []).

%% @doc Gets the `options' from the given `trail'.
-spec options(trail()) -> any().
options(Trail) ->
  maps:get(options, Trail, []).

%% @doc Gets the `metadata' from the given `trail'.
-spec metadata(trail()) -> map().
metadata(Trail) ->
  maps:get(metadata, Trail, #{}).

%% @doc Gets the `constraints' from the given `trail'.
-spec constraints(trail()) -> cowboy_router:constraints().
constraints(Trail) ->
  maps:get(constraints, Trail, []).

%% @doc This function allows you to define the routes on each resource handler,
%%      instead of defining them all in one place (as you're required to do
%%      with `cowboy'). Your handler must implement the callback `trails/0'
%%      and return the specific routes for that handler. That callback is
%%      invoked for each given module and then the results are concatenated.
-spec trails(module() | [module()]) -> trails:trails().
trails(Handlers) when is_list(Handlers) ->
  trails(Handlers, []);
trails(Handler) ->
  trails([Handler], []).

%% @doc Store the given list of trails.
-spec store(Trails::trails() |
              [{HostMatch::route_match(), Trails::trails()}]) -> ok.
store(Trails) ->
  store('_', Trails).

-spec store(Server::ranch:ref(),
            Trails::trails() |
              [{HostMatch::route_match(), Trails::trails()}]) -> ok.
store(_Server, []) ->
  ok;
store(Server, [{HostMatch, Trails} | Hosts]) ->
  NormalizedPaths = normalize_store_input(Trails),
  do_store(Server, HostMatch, NormalizedPaths),
  store(Server, Hosts);
store(Server, Trails) ->
  NormalizedPaths = normalize_store_input(Trails),
  do_store(Server, '_', NormalizedPaths).

%% @doc Retrieves all stored trails.
-spec all() -> [trail()].
all() ->
  all('_').

%% @doc Retrieves all stored trails for the given `HostMatch'
-spec all(HostMatch::route_match()) -> [trail()].
all(HostMatch) ->
  all('_', HostMatch).

%% @doc Retrieves all stored trails for the given `Server' and `HostMatch'
-spec all(Server::ranch:ref(), HostMatch::route_match()) -> [trail()].
all(Server, HostMatch) ->
  case application:get_application(trails) of
    {ok, trails} ->
      MatchSpec =
        case {Server, HostMatch} of
          {'_', HostMatch} ->
            [{{{'$1', HostMatch, '$2'}, '$3'}, [], ['$$']}];
          {Server, HostMatch} ->
            [{{{Server, HostMatch, '$1'}, '$2'}, [], ['$$']}]
        end,
      Matches = ets:select(trails, MatchSpec),
      FoundServers = [Srvr || [Srvr, _PathMatch, _Trail] <- Matches],
      % Extract unique elements
      Servers = lists:usort(FoundServers),
      % There should be no more than one element in this list.
      % If there is more than one, it means the same trail is defined
      % in other host(s).
      case Servers of
        [_, _ | _] -> throw(multiple_servers);
        _ -> ok
      end,
      Trails = all_trails(Matches, []),
      SortIdFun =
        fun(A, B) -> maps:get(trails_id, A) < maps:get(trails_id, B) end,
      SortedStoredTrails = lists:sort(SortIdFun, Trails),
      lists:map(fun remove_id/1, SortedStoredTrails);
    _ ->
      throw({not_started, trails})
  end.

%% @doc Fetch the trail that matches with the given path.
-spec retrieve(PathMatch::string()) -> trail() | notfound.
retrieve(PathMatch) ->
  retrieve('_', PathMatch).

%% @doc Fetch the trail that matches with the given host and path.
-spec retrieve(HostMatch::route_match(),
               PathMatch::string()) -> trail() | notfound.
retrieve(HostMatch, PathMatch) ->
  retrieve('_', HostMatch, PathMatch).

%% @doc Fetch the trail that matches with the given server and host and path.
-spec retrieve(Server::ranch:ref(),
               HostMatch::route_match(),
               PathMatch::string()) -> trail() | notfound.
retrieve(Server, HostMatch, PathMatch) ->
  case application:get_application(trails) of
    {ok, trails} ->
      Key = {Server, HostMatch, PathMatch},
      case ets:select(trails, [{{Key, '$1'}, [], ['$$']}]) of
        % No elements found
        [] -> notfound;
        % One element found
        [[Trail = #{path_match := PathMatch}]] -> remove_id(Trail);
        % More than one element found
        [_, _ | _] -> throw(multiple_trails)
      end;
    _ ->
      throw({not_started, trails})
  end.

%% @doc Get api_root env param value if any, empty otherwise.
-spec api_root() -> string().
api_root() ->
    application:get_env(trails, api_root, "").

%% @doc Set api_root env param to the given Path.
-spec api_root(string()) -> ok.
api_root(Path) ->
  application:set_env(trails, api_root, Path).

-spec servers() -> [ranch:ref()].
servers() ->
  lists:flatten(ets:match(ranch_server, {{conns_sup, '$1'}, '_'})).

-spec host_matches(ranch:ref()) -> [route_match()].
host_matches(ServerRef) ->
  Opts = lists:flatten(ets:match(ranch_server, {{opts, ServerRef}, '$1'})),
  Env = proplists:get_value(env, Opts, []),
  Dispatchs = proplists:get_value(dispatch, Env, []),
  lists:flatten([Host || {Host, _, _} <- Dispatchs]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
trails([], Acc) ->
  Acc;
trails([Module | T], Acc) ->
  trails(T, Acc ++ trails_handler:trails(Module)).

%% @private
-spec do_store(Server::ranch:ref(),
               HostMatch::route_match(),
               Trails::[route_path()]) -> ok.
do_store(_Server, _HostMatch, []) -> ok;
do_store(Server, HostMatch, [Trail = #{path_match := PathMatch} | Trails]) ->
  {ok, _} = application:ensure_all_started(trails),
  ets:insert(trails, {{Server, HostMatch, PathMatch}, Trail}),
  do_store(Server, HostMatch, Trails).

%% @private
all_trails([], Acc) ->
  Acc;
all_trails([[_, Trail] | T], Acc) ->
  all_trails(T, [Trail | Acc]);
all_trails([[_Server, _PathMatch, Trail] | T], Acc) ->
  all_trails(T, [Trail | Acc]).

%% @private
-spec normalize_store_input(trails()) -> [trail()].
normalize_store_input(RoutesPaths) ->
  normalize_id(normalize_paths(RoutesPaths)).

-spec normalize_id([route_path()]) -> trails().
normalize_id(Trails) ->
  Length = length(Trails),
  AddIdFun = fun(Trail, Id) -> Trail#{ trails_id => Id} end,
  lists:zipwith(AddIdFun, Trails, lists:seq(1, Length)).

%% @private
-spec normalize_paths(trails()) -> [trail()].
normalize_paths(RoutesPaths) ->
  [normalize_path(Path) || Path <- RoutesPaths].

%% @private
-spec remove_id(trail()) -> trail().
remove_id(Trail) -> maps:remove(trails_id, Trail).

%% @private
-spec normalize_path(route_path() | trail()) -> trail().
normalize_path({PathMatch, ModuleHandler, Options}) ->
  trail(PathMatch, ModuleHandler, Options);
normalize_path({PathMatch, Constraints, ModuleHandler, Options}) ->
  trail(PathMatch, ModuleHandler, Options, #{}, Constraints);
normalize_path(Trail) when is_map(Trail) -> Trail.
