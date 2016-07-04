%% Copyright 2016, Benoit Chesneau
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

-module(couch_httpd_metrics).

-export([handle_req/1]).

-include("db.hrl").


handle_req(#httpd{method='GET', path_parts=[_ | Path]}=Req) ->
    Metrics = barrel_metrics:list(),
    maybe_flush(Req, Metrics),

    Spec = case Path of
        [] -> [{{'_', '_', '_', '_'}, [], [true]}];
        [Section] ->
            [{{binary_to_atom(Section, latin1), '_', '_', '_'}, [], [true]}];
        [Section, Name |_] ->
            Section2 = binary_to_atom(Section, latin1),
            Name2 = binary_to_atom(Name, latin1),
            [{{Section2, Name2, '_', '_'}, [], [true]}]
    end,
    Compiled = ets:match_spec_compile(Spec),
    Obj = find(Metrics, Compiled, #{}),
    couch_httpd:send_json2(Req, Obj);
handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


find([Spec | Rest], Compiled, Obj) ->
    case ets:match_spec_run([Spec], Compiled) of
        [true] ->
            {S, N, _, _} = Spec,
            case exometer:get_value([S, N]) of
                {ok, Props} ->
                    V = case proplists:get_value(value, Props) of
                        undefined -> Props;
                        Val -> Val
                    end,
                    Obj2 = case maps:find(S, Obj) of
                                {ok, Items} ->
                                    Items2 = Items#{ N => V },
                                    Obj#{ S => Items2 };
                                error ->
                                    Obj#{ S => #{ N => V }}
                            end,
                    find(Rest, Compiled, Obj2);
                _ ->
                    find(Rest, Compiled, Obj)
            end;
        [] ->
            find(Rest, Compiled, Obj)
    end;
find([], _Compiled, Obj) ->
    Obj.

maybe_flush(Req, Metrics) ->
    case couch_httpd:qs_value(Req, "flush") of
        "true" ->
            lists:foreach(fun({S, N, _, _}) ->
                _ = exometer:sample([S, N])
            end, Metrics);
        _ ->
            ok
    end.
