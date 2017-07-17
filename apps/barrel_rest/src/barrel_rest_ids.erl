%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2017 12:38
%%%-------------------------------------------------------------------
-module(barrel_rest_ids).

%% API

-export([init/2]).
-export([terminate/3]).


init(Req, _Opts) ->
  Method = cowboy_req:method(Req),
  handle_request(Method, Req).

terminate(_Reason, _Req, _State) ->
  ok.

handle_request(<<"HEAD">>, Req0) ->
  OID = barrel:object_id(16),
  Req1 = cowboy_req:reply(
    200,
    #{ <<"X-Object-Id">> => OID },
    <<"">>,
    Req0
  ),
  {ok, Req1, nil};
handle_request(<<"GET">>, Req) ->
  #{ count := Count } = cowboy_req:match_qs([{count, int, 1}], Req),
  Ids = [barrel:object_id(16) || _I <- lists:seq(1, Count)],
  barrel_http_reply:json(#{ <<"ids">> => Ids }, Req, nil);
handle_request(_, Req) ->
  barrel_http_reply:error(405, Req, nil).
