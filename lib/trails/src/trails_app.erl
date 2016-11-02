%%% @hidden
-module(trails_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(term(), term()) -> {error, term()} | {ok, pid()}.
start(_Type, _Args) ->
  trails_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
