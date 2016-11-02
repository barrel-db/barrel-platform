-module(barrel_store_pool).
-author("Benoit Chesneau").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  mod :: atom(),
  mod_state :: any()
}).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.



%% @doc Starts and links a new process for the given store implementation.
-spec start_link(atom(), module(), [term()]) -> {ok, pid()}.
start_link(Name, Module, Options) ->
  PoolSize = maps:get(workers, Options, 100),
  WPoolConfigOpts = application:get_env(barrel, wpool_opts, []),
  WPoolOptions = [
    {overrun_warning, 5000},
    {overrun_handler, {barrel_lib, report_overrun}},
    {workers, PoolSize},
    {worker, {?MODULE, [Name, Module, Options]}}
  ],
  wpool:start_pool(Name, WPoolConfigOpts ++ WPoolOptions).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init([Name, Mod, Opts]) ->
  {ok, ModState} = Mod:init(Name, Opts),
  {ok, #state{mod=Mod, mod_state=ModState}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call({open_db, Name, Options}, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:open_db(ModState, Name, Options),
  {reply, Reply, State};

handle_call({clean_db, Name, DbId}, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:clean_db(Name, DbId, ModState),
  {reply, Reply, State};

handle_call({get_doc_info, DbId, DocId}, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:get_doc_info(DbId, DocId, ModState),
  {reply, Reply, State};

handle_call({write_doc, DbId, DocId, LastSeq, DocInfo, Body}, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:write_doc(DbId, DocId, LastSeq, DocInfo, Body, ModState),
  {reply, Reply, State};

handle_call({get_doc, DbId, DocId, Rev, WithHistory, MaxHistory, HistoryFrom}, _From,  State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:get_doc(DbId, DocId, Rev, WithHistory, MaxHistory, HistoryFrom, ModState),
  {reply, Reply, State};

handle_call({fold_by_id, DbId, Fun, AccIn, Opts}, _From,  State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:fold_by_id(DbId, Fun, AccIn, Opts, ModState),
  {reply, Reply, State};

handle_call({changes_since, DbId, Since, Fun, AccIn}, _From,
  State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:changes_since(DbId, Since, Fun, AccIn, ModState),
  {reply, Reply, State};

handle_call({last_update_seq, DbId}, _From, State) ->
  #state{ mod=Mod, mod_state=ModState} = State,
  Reply = Mod:last_update_seq(DbId, ModState),
  {reply, Reply, State};
handle_call(all_dbs, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:all_dbs(ModState),
  {reply, Reply, State};

handle_call({write_system_doc, DbId, DocId, Doc}, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:write_system_doc(DbId, DocId, Doc, ModState),
  {reply, Reply, State};

handle_call({read_system_doc, DbId, DocId}, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:read_system_doc(DbId, DocId, ModState),
  {reply, Reply, State};

handle_call({delete_system_doc, DbId, DocId}, _From, State=#state{ mod=Mod, mod_state=ModState}) ->
  Reply = Mod:delete_system_doc(DbId, DocId, ModState),
  {reply, Reply, State};


handle_call(_Request, _From, State) ->
  {reply, bad_call, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
