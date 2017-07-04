
-ifdef(PROFILE).
-define(BENCHMARK(Tag, Stuff), eqc_profile:benchmark({?MODULE, Tag}, fun() -> Stuff end)).
-define(BENCHMARK_GEN(Tag, Gen), eqc_profile:benchmark_gen({?MODULE, Tag}, fun() -> Gen end)).
-define(LOG_MAX(Tag, Val), eqc_profile:log(max, Tag, Val)).
-define(LOG_SUM(Tag, Val), eqc_profile:log(sum, Tag, Val)).
-else.
-define(BENCHMARK(_Tag, Stuff), Stuff).
-define(BENCHMARK_GEN(Tag, Gen), Gen).
-define(LOG_MAX(_Tag, _Val), ok).
-define(LOG_SUM(_Tag, _Val), ok).
-endif.


