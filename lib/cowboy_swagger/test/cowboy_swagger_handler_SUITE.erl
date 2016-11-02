-module(cowboy_swagger_handler_SUITE).

%% CT
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ handler_test/1
        , multiple_hosts_test/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  cowboy_swagger_test_utils:all(?MODULE).

-spec init_per_suite(
  cowboy_swagger_test_utils:config()
) -> cowboy_swagger_test_utils:config().
init_per_suite(Config) ->
  {ok, _} = shotgun:start(),
  Config.

-spec end_per_suite(
  cowboy_swagger_test_utils:config()
) -> cowboy_swagger_test_utils:config().
end_per_suite(Config) ->
  _ = shotgun:stop(),
  Config.

-spec init_per_testcase(TestCase::atom(),
                        Config::cowboy_swagger_test_utils:config()) ->
  cowboy_swagger_test_utils:config().
init_per_testcase(handler_test, Config) ->
  {ok, _} = example:start(),
  Config;
init_per_testcase(multiple_hosts_test, Config) ->
  {ok, _} = multiple_hosts_servers_example:start(),
  Config.

-spec end_per_testcase(TestCase::atom(),
                       Config::cowboy_swagger_test_utils:config()) ->
  cowboy_swagger_test_utils:config().
end_per_testcase(handler_test, Config) ->
  _ = example:stop(),
  ok = cleanup(),
  Config;
end_per_testcase(multiple_hosts_test, Config) ->
  _ = multiple_hosts_servers_example:stop(),
  ok = cleanup(),
  Config.

%% @private
-spec cleanup() -> ok.
cleanup() ->
  _ = application:stop(cowboy_swagger),
  _ = application:stop(trails),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handler_test(cowboy_swagger_test_utils:config()) -> {atom(), string()}.
handler_test(_Config) ->
  %% Expected result
  Trails = trails:trails([example_echo_handler,
                          example_description_handler,
                          cowboy_swagger_handler]),
  SanitizeTrails = cowboy_swagger:filter_cowboy_swagger_handler(Trails),
  ExpectedPaths = cowboy_swagger:dec_json(
    cowboy_swagger:enc_json(cowboy_swagger:swagger_paths(SanitizeTrails))),

  %% GET swagger.json spec
  ct:comment("GET /api-docs/swagger.json should return 200 OK"),
  #{status_code := 200, body := Body0} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger.json"),
  #{<<"swagger">> := <<"2.0">>,
    <<"info">> := #{<<"title">> := <<"Example API">>},
    <<"paths">> := ExpectedPaths} = cowboy_swagger:dec_json(Body0),

  %% GET index.html
  ct:comment("GET /api-docs should return 301 MOVED PERMANENTLY to " ++
             "/api-docs/index.html"),
  #{status_code := 301, headers := Headers} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs"),
  Location = {<<"location">>, <<"/api-docs/index.html">>},
  Location = lists:keyfind(<<"location">>, 1, Headers),

  %% GET swagger-ui.js - test /api-docs/[...] trail
  ct:comment("GET /api-docs/swagger-ui-js should return 200 OK"),
  #{status_code := 200, body := SwaggerUIBody} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs/swagger-ui.js"),
  {ok, SwaggerUIBodySrc} =
    file:read_file("../../../../priv/swagger/swagger-ui.js"),
  SwaggerUIBody = SwaggerUIBodySrc,

  %% GET unknown-file.ext - test /api-docs/[...] trail
  ct:comment("GET /api-docs/unknown-file.ext should return 404 NOT FOUND"),
  #{status_code := 404} =
    cowboy_swagger_test_utils:api_call(get, "/api-docs/unknown-file.ext"),
  {comment, ""}.

-spec multiple_hosts_test(_Config::cowboy_swagger_test_utils:config()) ->
  {atom(), string()}.
multiple_hosts_test(_Config) ->
  %% api1 - host1
  Trails11 = trails:trails(example_echo_handler),
  ExpectedPaths11 = get_expected_paths(Trails11),
  %% GET swagger.json spec from localhost:8383
  ct:comment("GET /api-docs/swagger.json should return 200 OK"),
  #{status_code := 200, body := Body11} =
    cowboy_swagger_test_utils:api_call(get,
                                       "/api-docs/swagger.json",
                                       "localhost",
                                       8383),
  #{<<"swagger">> := <<"2.0">>,
    <<"info">> := #{<<"title">> := <<"Example API">>},
    <<"paths">> := ExpectedPaths11} = cowboy_swagger:dec_json(Body11),
  %% api1 - host2
  Trails12 = trails:trails(host1_handler),
  ExpectedPaths12 = get_expected_paths(Trails12),
  %% GET swagger.json spec from 127.0.0.1:8383
  ct:comment("GET /api-docs/swagger.json should return 200 OK"),
  #{status_code := 200, body := Body12} =
    cowboy_swagger_test_utils:api_call(get,
                                       "/api-docs/swagger.json",
                                       "127.0.0.1",
                                       8383),
  #{<<"swagger">> := <<"2.0">>,
    <<"info">> := #{<<"title">> := <<"Example API">>},
    <<"paths">> := ExpectedPaths12} = cowboy_swagger:dec_json(Body12),
  %% api2 - host1
  Trails21 = trails:trails([host1_handler, example_echo_handler]),
  ExpectedPaths21 = get_expected_paths(Trails21),
  %% GET swagger.json spec from localhost:8282
  ct:comment("GET /api-docs/swagger.json should return 200 OK"),
  #{status_code := 200, body := Body21} =
    cowboy_swagger_test_utils:api_call(get,
                                       "/api-docs/swagger.json",
                                       "localhost",
                                       8282),
  #{<<"swagger">> := <<"2.0">>,
    <<"info">> := #{<<"title">> := <<"Example API">>},
    <<"paths">> := ExpectedPaths21} = cowboy_swagger:dec_json(Body21),
  {comment, ""}.

%% @private
-spec get_expected_paths(Trails::trails:trails()) -> jsx:json_term().
get_expected_paths(Trails) ->
  SanitizeTrails = cowboy_swagger:filter_cowboy_swagger_handler(Trails),
  cowboy_swagger:dec_json(
    cowboy_swagger:enc_json(cowboy_swagger:swagger_paths(SanitizeTrails))).
