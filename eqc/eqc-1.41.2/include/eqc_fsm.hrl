-import(eqc_fsm,[commands/1,commands/2,
                 run_commands/1, run_commands/2,run_commands/3,
                 state_after/1, state_after/2,
                 parallel_commands/1, parallel_commands/2,
                 run_parallel_commands/1, run_parallel_commands/2, run_parallel_commands/3,
                 state_names/1,
                 dot/1,visualize/1,analyze/1,
                 automate_weights/1]).

-import(eqc_statem,[pretty_commands/4, pretty_commands/5,
                    check_commands/3, check_commands/4,
                    more_commands/2,
                    command_names/1,
                    commands_length/1,
                    call_features/1,call_features/2,
                    more_bugs/1, more_bugs/2, more_bugs/3,
                    zip/2,
                    linearizable/1,
                    eq/2]).

-compile({parse_transform,eqc_export_records}).
-compile({parse_transform,eqc_group_commands}).

-define(VAR, '$var').

