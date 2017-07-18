-record(api_fun,    { name             :: atom()
                    , classify         :: any()
                    , arity            :: non_neg_integer()
                    , fallback = false :: boolean()
                    , matched = all    :: list(non_neg_integer()) | fun((any(), any()) -> boolean()) | all
                        % -- which arguments should match, or
                        % -- the more general matching function.
                    }).

-record(api_arg_c,
        { type                  :: atom() | string()
        , stored_type = default :: atom() | string()
                                    % -- we might want a different type in the argument struct
                                    % -- in this case a copy_in function is required
        , name                  :: atom() | string() | {atom(), string()}
        , dir         = in      :: in | out
        , buffer      = false   :: false         % -- is the argument a pointer to a buffer?
                                 | true
                                 | {true, non_neg_integer()}
                                 | {true, non_neg_integer(), string()}
        , phantom     = false   :: boolean()     % -- Phantom argument
        , matched     = false   :: boolean()     % -- Is the (in-)parameter matched during call
        , default_val = no      :: no | string() % -- Used for silent callouts
        , code        = no      :: no | string() % -- Code snippet, used for copying single param (in/out)
        }).

-record(api_fun_c,  { name           :: atom()                % -- C function name
                    , classify       :: any()
                    , ret = void     :: atom() | api_arg_c()  % -- return type
                    , args = []      :: [api_arg_c()]         % -- arguments
                    , silent = false :: false | {true, any()}
                    }).

-record(api_module, { name     :: atom()
                    , fallback :: atom()
                    , functions = [] :: [api_fun_erl()] | [api_fun_c()] }).

-record(api_spec,   { language = erlang     :: erlang | c  % The language we are mocking
                    , mocking = eqc_mocking :: atom()      % The mocking framework used
                    , config                :: any()       % Extra configuration data
                    , modules = []          :: [api_module()] }).

-type api_arg_c()   :: #api_arg_c{}.
-type api_fun_erl() :: #api_fun{}.
-type api_fun_c()   :: #api_fun_c{}.
-type api_spec()    :: #api_spec{}.
-type api_module()  :: #api_module{}.

-define(WILDCARD, '_').
-define(EXCEPTION(E), {'$eqc_exception', E}).

-define(SUCCESS, success).
-define(EVENT(M, F, As, R), {event, {{M, F}, As}, R}).
-define(XALT(L1, L2), {xalt, L1, L2}).
-define(PERM(Ls), {perm, Ls}).

-ifndef(SEQ).
-define(SEQ(L1, L2), {seq, L1, L2}).
-define(SEQ(Ls),     {seq, Ls}).
-endif.

-ifndef(PAR).
-define(PAR(L1, L2), {par, L1, L2}).
-define(PAR(Ls),     {par, Ls}).
-endif.

-ifndef(REPLICATE).
-define(REPLICATE(L), {repl, L}).
-endif.

