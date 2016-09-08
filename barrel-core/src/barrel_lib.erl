%% Copyright 2016, Benoit Chesneau
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

-module(barrel_lib).

-export([
  to_atom/1,
  to_binary/1,
  to_hex/1,
  hex_to_binary/1,
  report_overrun/1,
  uniqid/0, uniqid/1,
  parse_fold_options/1
]).

-include("barrel.hrl").

to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V) when is_binary(V) ->
  case catch binary_to_existing_atom(V, utf8) of
    {'EXIT', _} -> binary_to_atom(V, utf8);
    B -> B
  end;
to_atom(_) -> error(badarg).

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(_) -> error(badarg).

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) ->
    << <<(to_digit(H)),(to_digit(L))>> || <<H:4,L:4>> <= Bin >>;
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.

hex_to_binary(Bin) when is_binary(Bin) ->
  << <<(binary_to_integer( <<H, L>>, 16))>> || << H, L >> <= Bin >>.

report_overrun(Report) ->
  lager:error("~p", [Report]).

uniqid() -> uniqid(binary).

uniqid(string)    -> uuid:uuid_to_string(uuid:get_v4(), standard);
uniqid(binary)    -> uuid:uuid_to_string(uuid:get_v4(), binary_standard);
uniqid(integer)   -> <<Id:128>> = uuid:get_v4(), Id;
uniqid(float)     -> <<Id:128>> = uuid:get_v4(), Id * 1.0;
uniqid(_) -> error(badarg).


parse_fold_options(Opts) ->
  parse_fold_options(Opts, ?default_fold_options).

parse_fold_options([], Options) ->
  Options;
parse_fold_options([{start_key, Start} | Rest], Options)
  when is_binary(Start) or (Start =:= first) ->
  parse_fold_options(Rest, Options#{gte => Start});
parse_fold_options([{end_key, End} | Rest], Options)
  when is_binary(End) or (End == nil) ->
  parse_fold_options(Rest, Options#{lte => End});
parse_fold_options([{gt, GT} | Rest], Options)
  when is_binary(GT) or (GT =:= first) ->
  parse_fold_options(Rest, Options#{gt => GT});
parse_fold_options([{gte, GT} | Rest], Options)
  when is_binary(GT) or (GT =:= first) ->
  parse_fold_options(Rest, Options#{gte =>  GT});
parse_fold_options([{lt, LT} | Rest], Options)
  when is_binary(LT) or (LT == nil) ->
  parse_fold_options(Rest, Options#{lt => LT});
parse_fold_options([{lte, LT} | Rest], Options)
  when is_binary(LT) or (LT == nil) ->
  parse_fold_options(Rest, Options#{lte => LT});
parse_fold_options([{max, Max} | Rest], Options) ->
  parse_fold_options(Rest, Options#{max => Max});
parse_fold_options([{dir, fwd} | Rest], Options) ->
  parse_fold_options(Rest, Options#{move => next});
parse_fold_options([{dir, prev} | Rest], Options) ->
  parse_fold_options(Rest, Options#{move => prev});
parse_fold_options([_ | Rest], Options) ->
  parse_fold_options(Rest, Options).
