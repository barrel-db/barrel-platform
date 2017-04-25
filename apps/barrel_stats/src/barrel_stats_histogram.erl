%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2017 11:49
%%%-------------------------------------------------------------------
-module(barrel_stats_histogram).
-author("benoitc").
-behaviour(gen_server).

%% API
-export([
  create/2,
  set/3,
  reset/1,
  reset/2,
  reset_all/0,
  get_and_remove_raw_data/1,
  get_histogram/2,
  merge_histograms/2,
  merge_to/2,
  export/1
]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,  handle_info/2, terminate/2,  code_change/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(SIGNIFICANT_FIGURES, 3).
-define(HIGHEST_VALUE, 3600000000).

%%%===================================================================
%%% API
%%%===================================================================

create(Name, Labels) ->
  gen_server:call(?MODULE, {create, Name, Labels}).

set(_Name, #{ le := _ }, _Value) -> {error, invalid_tag};
set(Name, Labels, Value) when is_integer(Value), Value >= 0 ->
  Key = {Name, Labels},
  case erlang:get({barrel_hist_ref, Key}) of
    undefined ->
      Ref =
        try ets:lookup_element(barrel_histograms, Key, 2) of
          R -> R
        catch
          _:_ ->
            ok = create(Name, Labels),
            ets:lookup_element(barrel_histograms, Key, 2)
        end,
      erlang:put({barrel_hist_ref, Key}, Ref),
      hdr_histogram:record(Ref, Value);
    Ref ->
      hdr_histogram:record(Ref, Value)
  end;
set(Name, Labels, Value) when is_float(Value) ->
  set(Name, round(Value), Labels);
set(_Name, _Labels, Value) ->
  erlang:error({value_out_of_range, Value}).


reset(Name) ->
  Spec = ets:fun2ms(fun({{N, _}, Ref}) when N =:= Name -> Ref end),
  Refs = ets:select(barrel_histograms, Spec),
  _ = [hdr_histogram:reset(Ref) || Ref <- Refs],
  ok.

reset(Name, Labels) ->
  Key = {Name, Labels},
  case erlang:get({barrel_hist_ref, Key}) of
    undefined ->
      Ref = ets:lookup_element(?MODULE, Key, 2),
      erlang:put({barrel_hist_ref, Key}, Ref),
      hdr_histogram:reset(Ref);
    Ref ->
      hdr_histogram:reset(Ref)
  end.


get_and_remove_raw_data(Name) ->
  Spec = ets:fun2ms(fun({{N, _}, _, _}=Hist) when N =:= Name -> Hist end),
  Hists = ets:select(barrel_histograms, Spec),
  lists:foldl(
    fun ({{N, Labels}, Ref1, Ref2}, Acc) ->
      case hdr_histogram:rotate(Ref1, Ref2) of
        Bin when is_binary(Bin) ->
          [{N, Labels, Bin} | Acc];
        false ->
          Acc;
        {error, Reason} ->
          erlang:error({hdr_histogram_rotate_error, Reason})
      end
    end, [], Hists).

get_histogram(Data, Datapoints) ->
  {ok, Ref} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
  try
    ok = import_hdr_data(Ref, Data),
    case hdr_histogram:get_total_count(Ref) of
      K when K > 0 ->
        Stats = lists:map(
          fun (min) -> hdr_histogram:min(Ref);
            (max) -> hdr_histogram:max(Ref);
            (mean) -> hdr_histogram:mean(Ref);
            (median) -> hdr_histogram:median(Ref);
            (count) -> K;
            (sum) ->
              {ok,Itr} = hdr_iter:open(record, Ref, []),
              try
                Counts = hdr_iter:each(Itr,
                  fun({_,Attrs}, Acc) ->
                    
                    {value_from_index, C} = lists:keyfind(value_from_index, 1, Attrs),
                    [C|Acc]
                  end,
                  []),
                lists:sum(Counts)
              after
                hdr_iter:close(Itr)
              end;
            (N) when N =< 100 ->
              hdr_histogram:percentile(Ref, erlang:float(N));
            (N) when N =< 1000 ->
              hdr_histogram:percentile(Ref, N / 10)
          end, Datapoints),
        lists:zip(Datapoints, Stats);
      0 ->
        [{DP, null} || DP <- Datapoints]
    end
  after
    hdr_histogram:close(Ref)
  end.

merge_histograms(DataList, Datapoints) ->
  {ok, Ref} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
  try
    lists:foreach(fun (Values) -> import_hdr_data(Ref, Values) end, DataList),
    case hdr_histogram:get_total_count(Ref) of
      K when K > 0 ->
        Stats = lists:map(
          fun (min) -> hdr_histogram:min(Ref);
            (max) -> hdr_histogram:max(Ref);
            (mean) -> hdr_histogram:mean(Ref);
            (median) -> hdr_histogram:median(Ref);
            (count) -> hdr_histogram:get_total_count(Ref);
            (N) when N =< 100 ->
              hdr_histogram:percentile(Ref, erlang:float(N));
            (N) when N =< 1000 ->
              hdr_histogram:percentile(Ref, N / 10)
          end, Datapoints),
        lists:zip(Datapoints, Stats);
      0 ->
        [{DP, undefined} || DP <- Datapoints]
    end
  after
    hdr_histogram:close(Ref)
  end.

merge_to(undefined, DataList)->
  {ok, Ref} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
  merge_to(Ref, DataList);
merge_to(ToRef, DataList) ->
  lists:foreach(fun (Values) -> import_hdr_data(ToRef, Values) end, DataList),
  ToRef.

export(Ref) ->
  case hdr_histogram:to_binary(Ref, [{compression, none}]) of
    Bin when is_binary(Bin) -> {ok, Bin};
    {error, Reason} -> {error, Reason}
  end.

reset_all() ->
  Histograms = ets:tab2list(barrel_histograms),
  _ = [hdr_histogram:reset(Ref) ||{_, Ref} <- Histograms],
  ok.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{spawn_opt, [{priority, high}]}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  _ = ets:new(barrel_histograms,  [ordered_set, public, named_table, {read_concurrency, true}]),
  {ok, []}.

handle_call({create, Name, Labels}, _From, State) ->
  {reply, init_hist(Name, Labels), State};

handle_call(Req, _From, State) ->
  _ = lager:error("Unhandled call: ~p", [Req]),
  {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
  _ = lager:error("Unhandled cast: ~p", [Msg]),
  {stop, {unhandled_cast, Msg}, State}.

handle_info(Info, State) ->
  _ = lager:error("Unhandled info: ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init_hist(Name, Labels) ->
  {ok, Ref1} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
  {ok, Ref2} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
  case ets:insert_new(barrel_histograms, {{Name, Labels}, Ref1, Ref2}) of
    true -> ok;
    false ->
      hdr_histogram:close(Ref1),
      hdr_histogram:close(Ref2)
  end,
  ok.

import_hdr_data(To, BinHdrHistData) ->
  {ok, From} = hdr_histogram:from_binary(BinHdrHistData),
  _ = hdr_histogram:add(To, From),
  ok = hdr_histogram:close(From).