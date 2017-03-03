%% Copyright 2016, Bernard Notarianni
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_metrics).
-author("Bernard Notarianni").

-behaviour(gen_server).

-export([ tc/2
        , incr_counter/2
        , get_counter/1
        , reset_counters/0
        , reset_counter/1
        , metrics/0
        ]).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


-record(state, {statsd_server}).

counter_entries() ->
  [ docs_created
  , docs_read
  , docs_updated
  , docs_deleted
  , replication_doc_reads
  , replication_doc_read_failures
  , replication_doc_writes
  , replication_doc_write_failures
  ].


%% @doc equivalent to timer:tc/1 but storing results in metrics
%% to be implemented.
tc(Fun, _Entry) ->
  Fun().

get_counter(Entry) when is_atom(Entry) ->
  [{_, CntRef}] = ets:lookup(?MODULE, Entry),
  mzmetrics:get_resource_counter(CntRef, 0).

incr_counter(0, _) -> ok;
incr_counter(Val, Entry) when Val > 0 ->
  case get(Entry) of
    undefined ->
      try ets:lookup(?MODULE, Entry) of
          [{_, CntRef}] ->
          put(Entry, CntRef),
          incr_counter(Val, Entry);
          [] ->
          lager:error("invalid metric counter ~p", [Entry])
      catch
        _:_ ->
          %% we don't want to crash a session/queue
          %% due to an unavailable counter
          ok
      end;
    CntRef when Val == 1 ->
      mzmetrics:incr_resource_counter(CntRef, 0);
    CntRef ->
      mzmetrics:update_resource_counter(CntRef, 0, Val)
  end.

create(Entry) when is_atom(Entry) ->
  Ref = mzmetrics:alloc_resource(0, atom_to_list(Entry), 8),
  ets:insert(?MODULE, {Entry, Ref}).

reset_counters() ->
  lists:foreach(
    fun(Entry) ->
        reset_counter(Entry)
    end, counter_entries()).

reset_counter(Entry) ->
  [{_, CntRef}] = ets:lookup(?MODULE, Entry),
  mzmetrics:reset_resource_counter(CntRef, 0).

metrics() ->
  lists:foldl(fun(Entry, Acc) ->
                  [{Entry,
                    try get_counter(Entry) of
                        Value -> Value
                    catch
                      _:_ -> 0
                    end} | Acc]
              end, [], counter_entries()).

%% =============================================================================
%% gen_server API
%% =============================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  StatsdServer = application:get_env(barrel_store, statsd_server, undefined),
  {ok, _} = timer:send_interval(1000, publish_metrics),
  ets:new(?MODULE, [public, named_table, {read_concurrency, true}]),
  lists:foreach(
    fun(Entry) ->
        create(Entry)
    end, counter_entries()),
  {ok, #state{statsd_server=StatsdServer}}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(publish_metrics, State) ->
  hooks:run(metrics, [metrics()]),
  Host = <<"localhost">>,
  Probe = metrics,
  StatsdServer = State#state.statsd_server,
  push(Host, Probe, StatsdServer),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% =============================================================================
%% metric push handler
%% inspired from ejaberd mod_metrics.erl
%% =============================================================================

push(_, _, undefined) ->
  ok;
push(Host, Probe, StatsdServer) ->
  ct:print("push"),
  send_metrics(Host, Probe, StatsdServer).
  %% spawn(?MODULE, send_metrics, [Host, Probe, {127,0,0,1}, 11111]).

send_metrics(Host, Probe, {Peer, Port}) ->
  ct:print("senddd"),
  %% our default metrics handler is https://github.com/processone/grapherl
  %% grapherl metrics are named first with service domain, then nodename
  %% and name of the data itself, followed by type timestamp and value
  %% example => process-one.net/xmpp-1.user_receive_packet:c/1441784958:1
  [_, NodeId] = binary:split(atom_to_binary(node(), utf8), <<"@">>),
  [Node | _] = binary:split(NodeId, <<".">>),
  ct:print("ici host=~p node=~p",[Host, Node]),
  BaseId = <<Host/binary, "/", Node/binary, ".">>,
  DateTime = erlang:universaltime(),
  UnixTime = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
  TS = integer_to_binary(UnixTime),
  ct:print("avant open"),
  case gen_udp:open(0) of
	{ok, Socket} ->
      ct:print("send probe=~p",[Probe]),
	    case Probe of
        {Key, Val} ->
          BVal = integer_to_binary(Val),
          Data = <<BaseId/binary, (atom_to_binary(Key,utf8))/binary,
                   ":g/", TS/binary, ":", BVal/binary>>,
          ok = gen_udp:send(Socket, Peer, Port, Data),
            ct:print("ok");
        Key ->
          Data = <<BaseId/binary, (atom_to_binary(Key,utf8))/binary,
                   ":c/", TS/binary, ":1">>,
          ct:print("avant send"),
          ok = gen_udp:send(Socket, Peer, Port, Data),
          ct:print("ok")
	    end,
      ct:print("close"),
	    gen_udp:close(Socket);
    Error ->
      ct:print("error=~p",[Error]),
	    lagger:error("can not open udp socket to grapherl: ~p", [Error])
  end.
