-module(couch_httpd_metrics).

-export([handle_req/1]).

-include_lib("couch/include/couch_db.hrl").


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
    couch_log:info("spec is ~p~n", [Spec]),
    Compiled = ets:match_spec_compile(Spec),
    Obj = find(Metrics, Compiled, #{}),
    couch_log:info("got object, ~p~n", [Obj]),
    JsonObj = jsx:encode(Obj, [indent]),
    couch_httpd:send_json(Req, JsonObj);
handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


find([Spec | Rest], Compiled, Obj) ->
    case ets:match_spec_run([Spec], Compiled) of
        [true] ->
            {S, N, _, _} = Spec,
            case exometer:get_value([S, N]) of
                {ok, Props} ->
                    V = proplists:get_value(value, Props, <<>>),
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

to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_binary(V) -> V.
