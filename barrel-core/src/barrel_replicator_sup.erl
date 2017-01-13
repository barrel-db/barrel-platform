-module(barrel_replicator_sup).
-author("Benoit Chesneau").
-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Manager =
    #{id => barrel_replicate_manager,
      start => {barrel_replicate_manager, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [barrel_replicate_manager]},

  TaskSup =
    #{id => barrel_replicate_sup,
      start => {barrel_replicate_sup, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => supervisor,
      modules => [barrel_replicate_sup]},

  {ok, {{one_for_all, 10000, 1}, [Manager, TaskSup]}}.
