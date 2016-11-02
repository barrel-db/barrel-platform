{application, multiple_hosts_servers_example,
 [
  {description, "Cowboy Swagger Complex Example."},
  {vsn, "0.1"},
  {applications,
    [ kernel
    , stdlib
    , sasl

    , cowboy
    , trails
    , cowboy_swagger
    ]},
  {modules, []},
  {mod, {multiple_hosts_servers_example, []}},
  {start_phases, [{start_multiple_hosts_servers_example_http, []}]}
 ]
}.
