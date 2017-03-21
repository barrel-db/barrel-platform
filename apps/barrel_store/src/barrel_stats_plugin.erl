-module(barrel_stats_plugin).

-type metric_type() :: counter | gauge | duration.
-type metric_name() :: [ binary() ].
-type metric_env() :: [ any() ].

-export_type([ metric_type/0
             , metric_name/0
             , metric_env/0]).

-callback init(Type :: metric_type(), Name :: metric_name(), Env :: metric_env()) -> ok.

-callback increment( Name :: metric_name(), Value :: integer(), Env :: metric_env() ) -> ok.

-callback set_value( Name :: metric_name(), Value :: integer(), Env :: metric_env() ) -> ok.

-callback duration( Name :: metric_name(), Value :: integer(), Env :: metric_env() ) -> ok.
