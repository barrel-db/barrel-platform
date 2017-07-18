
-define(MATCH_TOKEN, '$eqc_callout_match').
-define(MATCH_GEN_TOKEN, '$eqc_callout_match_gen').
-define(MATCH(Pat, Exp), {?MATCH_TOKEN, Pat = Exp}).
-define(MATCH_GEN(Pat, Exp), {?MATCH_GEN_TOKEN, Pat = Exp}).
-define(CALLOUTS, '$eqc_callout_quote').

-ifndef(eqc_transform_callouts_no_transform).
-compile({parse_transform, eqc_transform_callouts}).
-endif.

