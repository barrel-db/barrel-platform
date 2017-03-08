-module(barrel_stats_plugin).

-type metric_type() :: counter | gauge.
-type metric_name() :: [ any() ].
-export_type([metric_name/0]).

-callback init(Type :: metric_type(), Name :: metric_name()) -> ok.

-callback increment( Name :: metric_name() ) -> ok.
