-module(barrel_replicate_sup).
-author("Benoit Chesneau").

%% API
-export([start_link/0]).

%% supervisor callback
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Spec =
    #{id => barrel_replicate,
      start => {barrel_replicate, start_link, []},
      restart => transient,
      shutdown => brutal_kill,
      type => worker,
      modules => [barrel_replicate]},
  
  {ok, {{simple_one_for_one, 5, 10}, [Spec]}}.
