%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

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
%%
%%

-export([main/1]).


main([]) ->
    JsFiles = ["priv/server/json2.js",
               "priv/server/filter.js",
               "priv/server/mimeparse.js",
               "priv/server/render.js",
               "priv/server/state.js",
               "priv/server/util.js",
               "priv/server/validate.js",
               "priv/server/views.js",
               "priv/server/script.js",
               "priv/server/loop.js"],

    CoffeeFiles = ["priv/server/json2.js",
                   "priv/server/filter.js",
                   "priv/server/mimeparse.js",
                   "priv/server/render.js",
                   "priv/server/state.js",
                   "priv/server/util.js",
                   "priv/server/validate.js",
                   "priv/server/views.js",
                   "priv/server/script.js",
                   "priv/server/coffee-script.js",
                   "priv/server/loop.js"],


    Concat = fun(Files, To) ->
            AccBin = lists:foldl(fun(Path, Acc) ->
                            {ok, Bin} = file:read_file(Path),
                            [Bin | Acc]
                    end, [], Files),
            FinalBin = iolist_to_binary(lists:reverse(AccBin)),
            file:write_file(To, FinalBin)
    end,

    ok = Concat(JsFiles, "priv/server/main.js"),
    ok = Concat(CoffeeFiles, "priv/server/main-coffee.js"),
    ok.
