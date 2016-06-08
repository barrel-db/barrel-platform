%% Copyright 2013-2016, Benoit Chesneau
%%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_dbupdates_httpd).

-export([handle_req/1]).

-include_lib("couch_db.hrl").
-include_lib("couch_httpd.hrl").

-record(state, {resp, feed}).

handle_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Qs = couch_httpd:qs(Req),
    Feed = proplists:get_value("feed", Qs, "longpoll"),

    Timeout = list_to_integer(
                proplists:get_value("timeout", Qs, "60000")
    ),

    Heartbeat0 = proplists:get_value("heartbeat", Qs),
    Heartbeat = case {Feed, Heartbeat0} of
        {"longpoll", _} -> false;
        {_, "false"} -> false;
        _ -> true
    end,

    Options = [{timeout, Timeout}, {heartbeat, Heartbeat}],

    {ok, Resp} = case Feed of
        "eventsource" ->
            Headers = [
                {"Content-Type", "text/event-stream"},
                {"Cache-Control", "no-cache"}
            ],
            couch_httpd:start_json_response(Req, 200, Headers);
        _ ->
            couch_httpd:start_json_response(Req, 200)
    end,

    State = #state{resp=Resp, feed=Feed},
    handle_dbupdates(fun handle_update/2,
                                     State, Options);

handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").

handle_update(stop, #state{resp=Resp}) ->
    couch_httpd:end_json_response(Resp);
handle_update(heartbeat, #state{resp=Resp}=State) ->
    {ok, Resp1} = couch_httpd:send_chunk(Resp, "\n"),
    {ok, State#state{resp=Resp1}};
handle_update(Event, #state{resp=Resp, feed="eventsource"}=State) ->
    EventObj = event_obj(Event),
    {ok, Resp1} = couch_httpd:send_chunk(Resp, ["data: ",
                                                ?JSON_ENCODE(EventObj),
                                                "\n\n"]),
    {ok, State#state{resp=Resp1}};
handle_update(Event, #state{resp=Resp, feed="continuous"}=State) ->
    EventObj = event_obj(Event),
    {ok, Resp1} = couch_httpd:send_chunk(Resp, [?JSON_ENCODE(EventObj) |
                            "\n"]),
    {ok, State#state{resp=Resp1}};
handle_update(Event, #state{resp=Resp, feed="longpoll"}) ->
    Props = event_obj(Event),
    JsonObj = Props#{<<"ok">> => true},
    couch_httpd:send_chunk(Resp, ?JSON_ENCODE(JsonObj)),
    stop.

event_obj({DbName, Type}) ->
    #{<<"type">> => barrel_lib:to_binary(Type), 
      <<"db_name">> => barrel_lib:to_binary(DbName)}.


handle_dbupdates(Fun, Acc, Options) ->
    _ = couch_event:subscribe(db_updated),
    try
        loop(Fun, Acc, Options)
    after
        couch_event:unsubscribe(db_updated)
    end.


loop(Fun, Acc, Options) ->
    [{timeout, Timeout}, {heartbeat, Heartbeat}] = Options,
    receive
        {couch_event, db_updated, Event} ->
            case Fun(Event, Acc) of
                {ok, Acc1} ->
                    loop(Fun, Acc1, Options);
                stop ->
                    Fun(stop, Acc)

            end
    after Timeout ->
        case Heartbeat of
            true ->
                case Fun(heartbeat, Acc) of
                    {ok, Acc1} ->
                        loop(Fun, Acc1, Options);
                    stop ->
                        Fun(stop, Acc)

                end;
            _ ->
                Fun(stop, Acc)
        end
    end.
