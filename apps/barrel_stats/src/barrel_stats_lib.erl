-module(barrel_stats_lib).

-export([
  now/0,
  metric_name/2,
  to_list/1,
  uniqid/0
]).


%% @doc return current time in seconds
-spec now() -> non_neg_integer().
now() ->
  erlang:system_time(seconds).

%% @doc convert a value to a list
-spec to_list(Value) -> List when
  Value :: list() | atom() | binary() | integer(),
List :: list().
to_list(V) when is_list(V) -> V;
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(_) -> erlang:error(badarg).

uniqid() ->
  integer_to_list(erlang:phash2(make_ref())).


%% @doc return a metric name with its labels as a text value
%% see https://prometheus.io/docs/instrumenting/exposition_formats/#text-format-details
-spec metric_name(Name, Labels) -> MetricName when
  Name :: string() | atom() | binary(),
Labels :: map(),
MetricName :: string().
metric_name(Name, Labels) ->
  PList = maps:to_list(Labels),
  LabelsStr = case PList of
                [] -> "";
                _ ->
                  KVs = [[to_list(K), "=", [$", to_list(V), $"]] || {K, V} <- lists:sort(PList)],
lists:flatten(["{", lists:join(",", KVs),  "}"])
end,
to_list(Name) ++ LabelsStr.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

metric_name_test() ->
  "test_metric" = metric_name("test_metric", #{}),
  "test_metric{method=\"post\"}" = metric_name("test_metric", #{method => <<"post">> }),
  "test_metric{method=\"post\",status=\"200\"}" = metric_name("test_metric", #{method => <<"post">>, status => 200 }),
  "test_metric{method=\"post\",status=\"200\"}" = metric_name("test_metric", #{status => 200, method => <<"post">>}).

-endif.