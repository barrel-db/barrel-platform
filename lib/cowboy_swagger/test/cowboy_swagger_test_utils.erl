-module(cowboy_swagger_test_utils).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).
-export([ api_call/2
        , api_call/4
        ]).

-type config() :: proplists:proplist().
-export_type([config/0]).

-spec all(atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite, group, all],
  Exports = apply(Module, module_info, [exports]),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

-spec api_call(atom(), string()) -> #{}.
api_call(Method, Uri) ->
  api_call(Method, Uri, "localhost", 8080).

-spec api_call(atom(), string(), string(), integer()) -> #{}.
api_call(Method, Uri, HostMatch, Port) ->
  {ok, Pid} = shotgun:open(HostMatch, Port),
  try
    Headers = #{}, Body = [],
    {ok, Response} = shotgun:request(Pid, Method, Uri, Headers, Body, #{}),
    Response
  after
    shotgun:close(Pid)
  end.
