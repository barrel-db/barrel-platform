-import(eqc_statem,[commands/1, commands/2, more_commands/2,
                    run_commands/1, run_commands/2,
                    run_commands/3, %% deprecated
                    pretty_commands/4, pretty_commands/5,
                    check_commands/3, check_commands/4,
                    postconditions/3,zip/2,command_names/1,
                    commands_length/1,
                    check_command_names/2,
                    check_command_names/3, %% deprecated
                    call_features/1,call_features/2,user_features/2,
                    state_after/1, state_after/2,
                    parallel_commands/1, parallel_commands/2,
                    run_parallel_commands/1, run_parallel_commands/2,
                    run_parallel_commands/3, %% deprecated
                    linearizable/1,
                    more_bugs/1, more_bugs/2, more_bugs/3,
                    eq/2,conj/1]).

-compile({parse_transform,eqc_export_records}).
-compile({parse_transform,eqc_group_commands}).

-define(VAR, '$var').

