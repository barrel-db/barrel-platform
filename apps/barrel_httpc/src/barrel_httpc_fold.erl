%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2017 14:59
%%%-------------------------------------------------------------------
-module(barrel_httpc_fold).
-author("benoitc").

%% API
-export([
  fold_by_id/4,
  fold_by_path/5,
  changes_since/5
]).

-export([
  init/1,
  handle_event/2,
  wait_rows/2,
  wait_rows1/2,
  wait_val/2,
  collect_object/2
]).

-define(TIMEOUT, 5000).

fold_by_id(#{pool := Pool} = Conn, UserFun, AccIn, Options) ->
  Headers =  case proplists:get_value(docids, Options) of
               undefined ->
                 [{<<"Content-Type">>, <<"application/json">>}];
               Ids when is_list(Ids) ->
                 [ {<<"Content-Type">>, <<"application/json">>} |
                    [{<<"x-barrel-id-match">>, Id} || Id <- Ids] ]
             end,

  Url = barrel_httpc_lib:make_url(Conn, <<"docs">>, proplists:delete(docids, Options)),
  ReqOpts = [{async, once}, {pool, Pool}],

  WrapperFun =
    fun(Obj, Acc) ->
      % extract metadata.
      #{ <<"doc">> := Doc, <<"meta">> := Meta} = Obj,
      UserFun(Doc, Meta, Acc)
    end,

  case hackney:request(<<"Get">>, Url, Headers, <<>>, ReqOpts) of
    {ok, Ref} ->
      wait_fold_response(Ref, WrapperFun, AccIn);
    Error ->
      Error
  end.

fold_by_path(#{pool := Pool}= Conn, Path, UserFun, AccIn, Options) when is_binary(Path) ->
  Url = barrel_httpc_lib:make_url(Conn, [<<"walk">>, Path], [{include_docs, true} |Options]),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  ReqOpts = [{async, once}, {pool, Pool}],
  IncludeDoc = proplists:get_value(include_docs, Options, false),
  WrapperFun =
    fun
      (#{ <<"doc">> := Doc} = Obj1, Acc) ->
        Val = barrel_jsonpointer:get(Path, Doc),
        Obj2 = case IncludeDoc of
                 true -> Obj1#{ <<"val">> => Val };
                 false -> maps:put(<<"val">>, Val, maps:remove(<<"doc">>, Obj1))
               end,
        UserFun(Obj2, Acc);
      (Obj, Acc) ->
        UserFun(Obj, Acc)
    end,
  
  case hackney:request(<<"GET">>, Url, Headers, <<>>, ReqOpts) of
    {ok, Ref} ->
      error_logger:info_msg("wait resp on ~p~n", [Url]),
      wait_fold_response(Ref, WrapperFun, AccIn);
    Error ->
      Error
  end;
fold_by_path(_Conn, _Path, _Fun, _AccIn, _Options) -> erlang:error(badarg).

changes_since(#{pool := Pool}= Conn, Since, Fun, AccIn, Options) ->
  SinceBin = integer_to_binary(Since),
  Url = barrel_httpc_lib:make_url(Conn, <<"docs">>, [{<<"since">>, SinceBin} | Options]),
  Headers = [
    {<<"Content-Type">>, <<"application/json">>},
    {<<"A-IM">>, <<"Incremental feed">>}
  ],
  ReqOpts = [{async, once}, {pool, Pool}],
  case hackney:request(<<"GET">>, Url, Headers, <<>>, ReqOpts) of
    {ok, Ref} ->
      wait_fold_response(Ref, Fun, AccIn);
    Error ->
      Error
  end.

wait_fold_response(Ref, Fun, AccIn) ->
  receive
    {hackney_response, Ref, {status, 200, _}} ->
      DecodeFun = jsx:decoder(?MODULE, [Fun, AccIn], [stream]),
      State =
        #{ref => Ref,
          decode_fun => DecodeFun,
          cb => Fun,
          acc => AccIn },
      loop(State);
    {hackney_response, Ref, {status, 404, _}} ->
      {error, not_found};
    {hackney_response, Ref, {status, Status, Reason}} ->
      {error, {http_error, Status, Reason}};
    {hackney_response, Ref, {error, Reason}} ->
      {error, Reason}
  after ?TIMEOUT ->
    {error, timeout}
  end.

loop(State = #{ ref := Ref}) ->
  ok = hackney:stream_next(Ref),
  receive
    {hackney_response, Ref, {headers, _Headers}} ->
      loop(State);
    {hackney_response, Ref, done} ->
      {error, bad_requet};
    {hackney_response, Ref, Data} when is_binary(Data) ->
      decode_data(Data, State);
    {hackney_response, Ref, Error} ->
      Error
  after ?TIMEOUT ->
    {error, timeout}
  end.

decode_data(Data, State = #{ref := Ref, decode_fun := DecodeFun}) ->
  try
      {incomplete, DecodeFun2} = DecodeFun(Data),
      try DecodeFun2(end_stream) of
          {done, Acc} ->
          _ = hackney:stop_async(Ref),
          ok = hackney:skip_body(Ref),
          {ok, Acc}
      catch
          error:badarg -> loop(State#{ decode_fun => DecodeFun2 })
      end
  catch
    error:badarg -> exit(badarg)
  end.

%%% json decoder %%%

init([Fun, AccIn]) ->
  #{ cb => Fun, acc => AccIn, next => fun wait_rows/2, ctx => nil}.

handle_event(end_json, #{ acc := Acc }) -> {done, Acc};
handle_event(Event, St = #{next := Fun}) -> Fun(Event, St).

wait_rows(start_object, St) -> St;
wait_rows(end_object, St) -> St;
wait_rows({key, <<"docs">>}, St) -> St#{ next => fun wait_rows1/2 };
wait_rows({key, <<"changes">>}, St) -> St#{ next => fun wait_rows1/2 }; %% current changes api
wait_rows({key, <<"count">>}, St) ->  St#{ next => fun wait_val/2 };
wait_rows({key, <<"last_seq">>}, St) ->  St#{ next => fun wait_val/2 }; %% current change api
wait_rows(_Other,  _) -> erlang:error(badarg).

wait_val({_, _}, St) -> St#{ next => fun wait_rows/2 }.

wait_rows1(start_array, St) -> St;
wait_rows1(start_object, St) -> St#{ next => fun collect_object/2, ctx => [#{}] };
wait_rows1(end_array, St) -> St#{ next => fun wait_rows/2 }.

collect_object(start_object, St = #{ctx := Ctx}) ->
  St#{ ctx => [#{} | Ctx] };
collect_object(end_object, St = #{ctx := [Obj, Key, Parent | Rest]}) when is_binary(Key) ->
  St#{ ctx => [ Parent#{ Key => Obj } | Rest ] };
collect_object(end_object, St = #{ctx := [Obj, List | Rest]}) when is_list(List) ->
  St#{ ctx => [ [Obj | List] | Rest ] };
collect_object(end_object, St = #{cb := Fun, acc := Acc, ctx := [Obj]}) when is_map(Obj) ->
  case Fun(Obj, Acc) of
    {ok, Acc2} -> St#{ acc => Acc2, next => fun wait_rows1/2, ctx => []};
    stop -> throw({stop, Acc});
    {stop, Acc2} -> throw({stop, Acc2})
  end;
collect_object(start_array, St = #{ctx := Ctx})->
  St#{ ctx => [[] | Ctx]};
collect_object(end_array, St = #{ctx := [A, Key, Obj|Rest]}) when is_list(A), is_binary(Key) ->
  St#{ ctx => [Obj#{ Key => lists:reverse(A) } | Rest] };
collect_object({key, Key}, St = #{ctx := Ctx}) ->
  St#{ ctx => [Key | Ctx]};
collect_object({_, Val}, St = #{ctx := [List|Rest]}) when is_list(List) ->
  St#{ ctx => [ [Val | List] | Rest] };
collect_object({_, Val}, St = #{ctx := [Key, Obj |Rest]}) when is_map(Obj), is_binary(Key) ->
  St#{ ctx => [ Obj#{ Key => Val } | Rest] }.
