-module(barrel_replicate_task_sup).
-author("Benoit Chesneau").

%% API
-export([start_link/0]).

%% supervisor callback
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Spec =
    #{id => barrel_replicate_task,
      start => {barrel_replicate_task, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [barrel_replicate_task]},

  {ok, {{simple_one_for_one, 5, 10}, [Spec]}}.
