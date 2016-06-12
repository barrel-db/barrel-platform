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
-module(barrel_websocket).

-export([broadcast/1, ws_loop/3]).

broadcast(Msg) ->
    Pids = gproc:lookup_values({p, l, {log_event_handler, log}}),
    lists:map(fun({_Pid, ReplyChannel}) -> ReplyChannel(Msg) end, Pids).

ws_loop([PayloadStr], Options, ReplyChannel) ->
    case jsx:is_json(PayloadStr) of
    true -> Payload = jsx:decode(PayloadStr),
        Type = proplists:get_value(<<"type">>,Payload),
        Resource = proplists:get_value(<<"resource">>, Payload),
        case {Type, Resource} of
            {<<"subscribe">>, <<"logs">>} -> 
                gproc:reg({p, l, {log_event_handler, log}}, ReplyChannel),
                ReplyChannel("Subscribed to logs");
            _ -> 
                ReplyChannel("Type and/or resource not present")
        end;
    false -> 
        ReplyChannel("Invalid request")
    end,
    Options.