
%% Created by benoitc on 13/09/16.

-module(barrel_store).
-author("Benoit Chesneau").

%% API
-export([
  open_db/3,
  clean_db/3,
  all_dbs/1,
  get_doc_info/3,
  write_doc/6,
  get_doc/7,
  fold_by_id/5,
  changes_since/5,
  last_update_seq/2,
  write_system_doc/4,
  read_system_doc/3,
  delete_system_doc/3
]).

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

-define(DEFAULT_WORKERS, 100).

-record(state, {
  mod :: atom(),
  mod_state :: any()
}).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.


%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback init(atom(), term()) -> {ok, any()}.

-callback open_db(term(), term(), list()) -> term().

-callback clean_db(term(), term(), term()) -> ok | {error, term()}.

-callback all_dbs(term()) -> list().

-callback get_doc_info(term(), binary(), binary()) -> {ok, map()} | {error, term()}.

-callback write_doc(term(), binary(), binary(), integer(), map(), map())
    -> {ok, integer()} | {error, term()}.

-callback get_doc(DbId :: binary(), DocId :: binary(), Rev :: barrel_db:rev(),
  History :: boolean(), MaxHistory::integer(), HistoryForm::list(), State :: any())
    -> {ok, Body :: map() } | {error, term()}.


-callback fold_by_id(DbId :: binary(), Fun :: fun(),
  AccIn::any(), FoldOpts :: list(), State::any()) -> AccOut :: any().

-callback changes_since(DbId :: binary(), Since :: integer(), Fun :: fun(),
  AccIn::any(), State::any()) -> AccOut :: any().

-callback last_update_seq(DbId :: binary(), State :: any()) ->
  Seq :: integer | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

open_db(Store, Name, Options) ->
  call(Store, {open_db, Name, Options}).

clean_db(Store, Name, DbId) ->
  call(Store, {clean_db, Name, DbId}).

all_dbs(Store) ->
  call(Store, all_dbs).

get_doc_info(Store, DbId, DocId) ->
  call(Store, {get_doc_info, DbId, DocId}).

write_doc(Store, DbId, DocId, LastSeq, DocInfo, Body) ->
  call(Store, {write_doc, DbId, DocId, LastSeq, DocInfo, Body}).

get_doc(Store, DbId, DocId, Rev, WithHistory, MaxHistory, HistoryFrom) ->
  call(Store, {get_doc, DbId, DocId, Rev, WithHistory, MaxHistory, HistoryFrom}).


fold_by_id(Store, DbId, Fun, AccIn, Opts) ->
  call(Store, {fold_by_id, DbId, Fun, AccIn, Opts}).

changes_since(Store, DbId, Since, Fun, AccIn) ->
  call(Store, {changes_since, DbId, Since, Fun, AccIn}).

last_update_seq(Store, DbId) ->
  call(Store, {last_update_seq, DbId}).


call(Store, Msg) ->
  try wpool:call(Store, Msg)
  catch
    _:Error -> {error, {store_error, {Store, Error}}}
  end.

write_system_doc(Store, DbId, DocId, Doc) ->
  wpool:call(Store, {write_system_doc, DbId, DocId, Doc}).

read_system_doc(Store, DbId, DocId) ->
  wpool:call(Store, {read_system_doc, DbId, DocId}).

delete_system_doc(Store, DbId, DocId) ->
  wpool:call(Store, {delete_system_doc, DbId, DocId}).

%% @doc Starts and links a new process for the given store implementation.
-spec start_link(atom(), module(), [term()]) -> {ok, pid()}.
start_link(Name, Module, Options) ->
  PoolSize = proplists:get_value(workers, Options, ?DEFAULT_WORKERS),
  _ = code:ensure_loaded(Module),
  case erlang:function_exported(Module, pre_start, 2) of
    false ->
      lager:info("function pre_start not exported", []),
      ok;
    true -> Module:pre_start(Name, Options)
  end,

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
  io:format("request is ~p~nstate is ~p~n", [_Request, State]),
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

