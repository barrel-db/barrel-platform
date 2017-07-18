-import(eqc_component,[commands/1, commands/2, adapt_commands/2,
                       run_commands/1, run_commands/2, run_commands/3,
                       state_after/1, state_after/2, get_return_value/1,
                       pretty_commands/4, pretty_commands/5, pretty_commands/6
                      ]).

-import(eqc_statem,[ more_commands/2,
                     command_names/1, check_command_names/2, check_command_names/3,
                     commands_length/1,
                     eq/2,
                     more_bugs/1, more_bugs/2, more_bugs/3,
                     call_features/1,
                     call_features/2
                   ]).

-compile({parse_transform,eqc_export_records}).
-compile({parse_transform,eqc_group_commands}).

-include_lib("eqc/include/eqc_callout_lang.hrl").
-include_lib("eqc/include/eqc_mocking.hrl").
-include_lib("eqc/include/eqc_transform_callouts.hrl").


