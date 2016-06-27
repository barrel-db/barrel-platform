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

-export([start/1, stop/1]).

%% hooks
-export([
  on_http_request/2,
  on_http_response/4,
  after_doc_read/2,
  before_doc_update/2
]).

-include("log.hrl").

start(_Opts) ->
  {ok, _} = application:ensure_all_started(exometer_core),

  hooks:reg(?MODULE, on_http_request, 2),
  hooks:reg(?MODULE, on_http_response, 4),
  hooks:reg(?MODULE, after_doc_read, 2),
  hooks:reg(?MODULE, before_doc_update, 2),

  load_metrics().

stop(_Opts) -> ok.

on_http_request(Req, Env) ->
  {Method, _} = cowboy_req:method(Req),
  exometer:update([barrel, requests], 1),
  exometer:update([httpd_request_methods, Method], 1),
  Req2 = cowboy_req:set_meta(start_time, erlang:timestamp(), Req),
  {ok, Req2, Env}.

on_http_response(Status, _Headers, _Body, nil) ->
  Start = erlang:get(request_start_time),
  RequestTime = round(timer:now_diff(os:timestamp(), Start)/1000),
  exometer:update([httpd_status_codes, Status], 1),
  exometer:update([barrel, request_time], RequestTime);
on_http_response(Status, _Headers, _Body, Req) ->
  {Start, _} = cowboy_req:meta(start_time, Req),
  RequestTime = round(timer:now_diff(os:timestamp(), Start)/1000),
  exometer:update([httpd_status_codes, Status], 1),
  exometer:update([barrel, request_time], RequestTime),
  Req.

after_doc_read(_Doc, _Db) ->
  _ = exometer:update([barrel, database_reads], 1),
  ok.

before_doc_update(_Doc, _Db) ->
  _ = exometer:update([barrel, database_writes], 1),
  ok.

load_metrics() ->
  Path = filename:join(barrel_lib:priv_dir(), "stats_descriptions.cfg"),
  case file:consult(Path) of
    {ok, Specs} ->
      case validate_specs(Specs) of
        ok -> init_metrics(Specs);
        bad_spec ->
          ?log(error, "stats_descriptions.cfg is invalid.~n", []),
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
  bad_spec.
