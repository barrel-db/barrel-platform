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
%
-module(couch_httpd_util).

-export([display_uris/0, display_uris/1,
         write_uri_file/0,
         get_listeners/0,
         get_uri/2,
         get_scheme/1,
         get_port/1]).


-export([parse_boolean/1]).
-export([parse_int/1]).
-export([parse_pos_int/1]).
-export([parse_json/2]).


-include_lib("couch/include/couch_db.hrl").


display_uris() ->
    display_uris(get_listeners()).

display_uris(Bindings) ->
    Ip = barrel_config:get("httpd", "bind_address", "127.0.0.1"),
    lists:foreach(fun(Binding) ->
                Uri = get_uri(Binding, Ip),
                barrel_log:info("HTTP API started on ~p~n", [Uri])
        end, Bindings).

write_uri_file() ->
    Ip = barrel_config:get("httpd", "bind_address", "127.0.0.1"),
    Listeners = get_listeners(),
    Uris = [get_uri(Name, Ip) || Name <- Listeners],
    case barrel_config:get("couchdb", "uri_file", null) of
        null -> ok;
        UriFile ->
            Lines = [begin case Uri of
                            undefined -> [];
                            Uri -> io_lib:format("~s~n", [Uri])
                        end end || Uri <- Uris],
            case file:write_file(UriFile, Lines) of
                ok -> ok;
                {error, eacces} ->
                    barrel_log:info("Permission error when writing to URI file ~s",
                              [UriFile]),
                    throw({file_permission_error, UriFile});
                Error2 ->
                    barrel_log:info("Failed to write to URI file ~s: ~p~n",
                              [UriFile, Error2]),
                    throw(Error2)
            end
    end.

get_listeners() ->
    SchemeStr = barrel_config:get("httpd", "scheme", "http"),
    SchemeList = re:split(SchemeStr, "\\s*,\\s*",[{return, list}]),

    lists:foldl(fun(S, Acc) ->
                [list_to_atom("couch_" ++ S) | Acc]
        end, [], lists:reverse(SchemeList)).

get_uri(Name, Ip) ->
    Port = get_port(Name),
    Scheme = get_scheme(Name),
    Scheme ++ "://" ++ Ip ++ ":" ++ integer_to_list(Port) ++ "/".

get_scheme(couch_http) -> "http";
get_scheme(couch_https) -> "https".

get_port(Ref) ->
    try
        mochiweb_socket_server:get(Ref, port)
    catch
        exit:{noproc, _} -> undefined
    end.


parse_boolean(true) ->
    true;
parse_boolean(false) ->
    false;
parse_boolean(Val) when is_binary(Val) ->
    parse_boolean(binary_to_list(Val));
parse_boolean(Val) ->
    case string:to_lower(Val) of
    "true" -> true;
    "false" -> false;
    _ ->
        Msg = io_lib:format("Invalid boolean parameter: ~p", [Val]),
        throw({query_parse_error, list_to_binary(Msg)})
    end.

parse_int(Val) when is_integer(Val) ->
    Val;
parse_int(Val) ->
    case (catch list_to_integer(Val)) of
    IntVal when is_integer(IntVal) ->
        IntVal;
    _ ->
        Msg = io_lib:format("Invalid value for integer: ~p", [Val]),
        throw({query_parse_error, list_to_binary(Msg)})
    end.

parse_pos_int(Val) ->
    case parse_int(Val) of
    IntVal when IntVal >= 0 ->
        IntVal;
    _ ->
        Fmt = "Invalid value for positive integer: ~p",
        Msg = io_lib:format(Fmt, [Val]),
        throw({query_parse_error, list_to_binary(Msg)})
    end.

parse_json(V, false) when is_list(V) ->
    ?JSON_DECODE(V);
parse_json(V, _) ->
    V.
