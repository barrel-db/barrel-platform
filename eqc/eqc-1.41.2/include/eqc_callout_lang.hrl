-ifndef(EQC_CALLOUT_LANG).
-define(EQC_CALLOUT_LANG,true).

-define(CALLOUT(M, F, A, Res), eqc_component:callout(M, F, A, Res)).
-define(SEND(Pid, Msg), ?CALLOUT(erlang, send, [Pid, Msg], Msg)).
-define(SELFCALL(F, As), {self_callout, ?MODULE, F, As}).
-define(SELFCALL(M, F, As), {self_callout, M, F, As}).
-define(APPLY(F, As), ?SELFCALL(F, As)).
-define(APPLY(M, F, As), ?SELFCALL(M, F, As)).
-define(RET(X), {return, X}).
-define(FAIL(E), {fail, E}).
-define(ASSERT(M, F, A, Err), {assert, M, F, A, Err}).
-define(ASSERT(M, F, A), ?ASSERT(M, F, A, {assertion_failed, M, F, A, {?FILE, ?LINE}})).
%% LET is already an EQC macro
-define(SAFELET(X, G, R), {safe_let, fun(X) -> R end, ?LET(X, G, {val, X, R})}).
-define(BIND(Pat, Callout, Body), {bind, Callout, fun(Pat) -> Body end}).
-define(VAR, '$var').
-define(SELF, '$self').
-define(SEQ(L1, L2), {seq, L1, L2}).
-define(SEQ(Ls), {seq, Ls}).
-define(PAR(L1, L2), {par, L1, L2}).
-define(PAR(Ls), {par, Ls}).
-define(EITHER(L1, L2), {xalt, L1, L2}).
-define(EITHER(Tag, L1, L2), {xalt, Tag, L1, L2}).
-define(REPLICATE(L), {repl, L}).
-define(OPTIONAL(L), ?EITHER(L, ?EMPTY)).
-define(OPTIONAL(Tag, L), ?EITHER(Tag, L, ?EMPTY)).
-define(EMPTY, empty).
-define(BLOCK(X), {'$eqc_block', X}).
-define(BLOCK, ?BLOCK(?SELF)).
-define(UNBLOCK(X, R), {unblock, X, R}).
-define(WHEN(Cond, Callout), case Cond of true -> Callout; false -> ?EMPTY end).

-endif.


