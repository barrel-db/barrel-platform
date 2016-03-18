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

-module(couch_ejson_compare).

-export([less/2, less_json_ids/2, less_json/2]).

less_json_ids({JsonA, IdA}, {JsonB, IdB}) ->
    case less(JsonA, JsonB) of
    0 ->
        IdA < IdB;
    Result ->
        Result < 0
    end.

less_json(A,B) ->
    less(A, B) < 0.

less(A,A)                                 -> 0;

less(A,B) when is_atom(A), is_atom(B)     -> atom_sort(A) - atom_sort(B);
less(A,_) when is_atom(A)                 -> -1;
less(_,B) when is_atom(B)                 -> 1;

less(A,B) when is_number(A), is_number(B) -> A - B;
less(A,_) when is_number(A)               -> -1;
less(_,B) when is_number(B)               -> 1;

less(A,B) when is_binary(A), is_binary(B) -> ucol:compare(A,B);
less(A,_) when is_binary(A)               -> -1;
less(_,B) when is_binary(B)               -> 1;

less(A,B) when is_list(A), is_list(B)     -> less_list(A,B);
less(A,_) when is_list(A)                 -> -1;
less(_,B) when is_list(B)                 -> 1;

less({A},{B}) when is_list(A), is_list(B) -> less_props(A,B);
less({A},_) when is_list(A)               -> -1;
less(_,{B}) when is_list(B)               -> 1.

atom_sort(null) -> 1;
atom_sort(false) -> 2;
atom_sort(true) -> 3.

less_props([], [_|_]) ->
    -1;
less_props(_, []) ->
    1;
less_props([{AKey, AValue}|RestA], [{BKey, BValue}|RestB]) ->
    case ucol:compare(AKey, BKey) of
    0 ->
        case less(AValue, BValue) of
        0 ->
            less_props(RestA, RestB);
        Result ->
            Result
        end;
    Result ->
        Result
    end.

less_list([], [_|_]) ->
    -1;
less_list(_, []) ->
    1;
less_list([A|RestA], [B|RestB]) ->
    case less(A,B) of
    0 ->
        less_list(RestA, RestB);
    Result ->
        Result
    end.
