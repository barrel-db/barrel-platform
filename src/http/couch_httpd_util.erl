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
%
-module(couch_httpd_util).

-export([parse_boolean/1]).
-export([parse_int/1]).
-export([parse_pos_int/1]).
-export([parse_json/2]).

-include_lib("couch_db.hrl").


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

parse_json(V, false) when is_list(V)->
  ?JSON_DECODE(list_to_binary(V));
parse_json(V, _) ->
    V.
