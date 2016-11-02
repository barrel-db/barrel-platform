{application, example,
 [
  {description, "Cowboy Swagger Basic Example."},
  {vsn, "0.1"},
  {applications,
   [kernel,
    stdlib,
    cowboy,
    trails,
    cowboy_swagger
   ]},
  {modules, []},
  {mod, {example, []}},
  {start_phases, [{start_trails_http, []}]}
 ]
}.
