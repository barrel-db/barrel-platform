%% Copyright (c) 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.
%%


-module(barrel_metrics).

-export([start/0]).

start() ->
  load_metrics().

load_metrics() ->
  Path = filename:join(barrel_lib:priv_dir(), "stats_descriptions.cfg"),
  case file:consult(Path) of
    {ok, Specs} ->
      case validate_specs(Specs) of
        ok -> init_metrics(Specs);
        bad_spec ->
          lager:error("stats_descriptions.cfg is invalid.~n", []),
          ok
      end;
    {error, _Error} -> error
  end.

init_metrics(Specs) ->
  lists:foreach(fun({Section, Name, Type, _Desc}) ->
    _ = exometer:new([Section, Name], Type)
                end, Specs),
  ok.

validate_specs([]) ->
  ok;
validate_specs([{_Section, _Name, Type, ShortDesc} | Rest])
  when is_atom(Type), is_list(ShortDesc) ->
  validate_specs(Rest);
validate_specs([_|_Rest]) ->
  bad_metric.
