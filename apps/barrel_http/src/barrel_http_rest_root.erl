%% Copyright 2016, Bernard Notarianni
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

-module(barrel_http_rest_root).
-author("Bernard Notarianni").


%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Information about Barrel-DB version"
               , produces => ["application/json"]
               }
     },
  [trails:trail("/", ?MODULE, [], Metadata)].

-record(state, {method, store, dbid, conn}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {ok, Vsn} = application:get_key(barrel_http, vsn),
  {ok, Description} = application:get_key(barrel_http, description),
  Doc=#{ description => list_to_binary(Description)
       , version => list_to_binary(Vsn)
       , swagger => <<"/api-doc">>
       },
  barrel_http_reply:doc(Doc, Req, State).

terminate(_Reason, _Req, _State) ->
  ok.


