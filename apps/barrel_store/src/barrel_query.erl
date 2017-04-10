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

-module(barrel_query).

-export([query/6]).

-include("barrel_store.hrl").


-define(DEFAULT_MAX, 10000).


query(Db, Path, Fun, Acc, Type, Opts) ->
  _ = lager:info(
        "query(~p,~p,~p,~p,~p,~p)",
        [Db, Path, Fun, Acc, Type, Opts]
       ),


  Acc.


encode_fun(fwd) ->
  Prefix =  barrel_keys:prefix(idx_forward_path),
  Fun = fun(Path) ->
            barrel_keys:encode_path_forward(Prefix, Path)
        end,
  Fun;
encode_fun(rev) ->
  Prefix = barrel_keys:prefix(idx_reverse_path),
  Fun = fun(Path) ->
            barrel_keys:encode_path_reverse(Prefix, Path)
        end,
  Fun.

normalize_path(<< $$, _/binary >>=Path) -> Path;
normalize_path(<< $/, _/binary >>=Path) -> << $$, Path/binary >>;
normalize_path(Path) when is_binary(Path)-> << $$, $/, Path/binary >>;
normalize_path(_) -> erlang:error(badarg).

partial_path(Path0) ->
  Path1 = normalize_path(Path0),
  Parts = binary:split(Path1, <<"/">>, [global]),
  partial_path(Parts, length(Parts)).

partial_path(Parts, Len) when Len =< 3 -> Parts;
partial_path(Parts, Len) -> lists:sublist(Parts, Len - 2, Len).
