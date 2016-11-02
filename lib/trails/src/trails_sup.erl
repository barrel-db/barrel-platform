%%% @hidden
-module(trails_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, 10, 60}, []}}.
init([]) ->
  init_ets_table(),
  {ok, {{one_for_one, 10, 60}, []}}.

%% @private
-spec init_ets_table() -> atom().
init_ets_table() ->
  ets:new(trails, [public, named_table]).
