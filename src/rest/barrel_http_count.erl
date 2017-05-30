%% Copyright 2017, Bernard Notarianni
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

-module(barrel_http_count).
-author("Bernard Notarianni").
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).


-record(state, {
          next,
          req,
          status_code,
          start_time
         }).

init(StreamID, Req, Opts) ->
  ok = hooks:run(barrel_http_in, [Req]),
	{Commands0, Next} = cowboy_stream:init(StreamID, Req, Opts),
  fold(Commands0, #state{req=Req, next=Next, start_time=erlang:monotonic_time()}).

data(StreamID, IsFin, Data, State0=#state{next=Next0}) ->
	{Commands0, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
  fold(Commands0, State0#state{next=Next}).

info(StreamID, Info, State0=#state{next=Next0}) ->
	{Commands0, Next} = cowboy_stream:info(StreamID, Info, Next0),
  fold(Commands0, State0#state{next=Next}).

terminate(StreamID, Reason, #state{next=Next}=State) ->
  #state{status_code=StatusCode, req=Req, start_time=T1} = State,
  T2 = erlang:monotonic_time(),
  ok = hooks:run(barrel_http_out, [Req, StatusCode, T2 - T1]),
  cowboy_stream:terminate(StreamID, Reason, Next).

early_error(_StreamID, _Reason, _PartialReq, Resp, _Opts) ->
  Resp.

fold(Commands, State) ->
  fold(Commands, State, []).

fold([], State, Acc) ->
  {lists:reverse(Acc), State};

fold([Command={response, Code, _Headers, _Body}|Tail], State, Acc) ->
  fold(Tail, State#state{status_code=Code}, [Command|Acc]);

fold([Command={headers, Code, _Headers}|Tail], State, Acc) ->
  fold(Tail, State#state{status_code=Code}, [Command|Acc]);

fold([Command={error_response, Code, _Headers, _}|Tail], State, Acc) ->
  fold(Tail, State#state{status_code=Code}, [Command|Acc]);

fold([Command|Tail], State, Acc) ->
  fold(Tail, State, [Command|Acc]).
