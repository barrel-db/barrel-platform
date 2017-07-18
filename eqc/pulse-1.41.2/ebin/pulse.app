{application,pulse,
             [{vsn,"1.41.2"},
              {modules,[pulse,pulse_sup,pulse_event,pulse_event_terminal,
                        pulse_event_graph,pulse_instrument,pulse_scheduler]},
              {mod,{pulse,[]}},
              {description,"ProTest User Level Scheduler for Erlang"},
              {registered,[pulse,pulse_sup,pulse_event]},
              {applications,[kernel,stdlib]},
              {env,[{run_timeout,30000}]}]}.
