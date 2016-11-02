-module(host1_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {example_default,
         [
          init/3,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export([ allowed_methods/2
        , handle_get/2]).

%trails
-behaviour(trails_handler).
-export([trails/0]).

trails() ->
  Metadata =
    #{get =>
      #{tags => ["whoami"],
        description => "Get hostname",
        produces => ["text/plain"]
      }
    },
  [trails:trail(
    "/whoami",
    host1_handler,
    #{},
    Metadata)].

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% internal
handle_get(Req, State) ->
  {Host, Req1} = cowboy_req:host(Req),
  Body = <<"I am ", Host/binary>>,
  {Body, Req1, State}.
