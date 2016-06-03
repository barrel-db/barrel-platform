% 2011-2012 (c) Benoît Chesneau <benoitc@refuge.io>
%
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

-module(couch_randomdoc).

-include_lib("couch_db.hrl").
-include_lib("couch_mrview.hrl").

-export([random_doc/1, random_doc/2, random_doc/3,
         random_view_doc/3]).

random_doc(Db) ->
    DefaultFun = fun(_Doc) -> true end,
    random_doc(Db, DefaultFun, []).

random_doc(Db, FilterFun) ->
    random_doc(Db, FilterFun, []).


random_doc(Db, FilterFun, Opts) ->
    N = {ok, Info} = couch_db:get_db_info(Db),
    case couch_util:get_value(doc_count, Info) of
        C when C < 1 -> C;
        C -> crypto:rand_uniform(0, C)
    end,

    Fun = fun
        (#full_doc_info{}, _O, Skip) when Skip > 0 ->
            {ok, Skip-1};
        (#full_doc_info{} = FullDocInfo, _O, Acc) ->
            case couch_doc:to_doc_info(FullDocInfo) of
                #doc_info{revs=[#rev_info{deleted=true}|_]} ->
                    {ok, Acc};
                DocInfo ->
                    Doc = couch_doc:load(Db, DocInfo, Opts),

                    case FilterFun(Db, Doc) of
                        true ->
                            {stop, Doc};
                        false ->
                            {ok, Acc}
                    end
            end;
        (_Other, _, Acc) ->
            {stop, Acc}
    end,
    {ok, _, Result} = couch_db:enum_docs(Db, Fun, N, []),
    finish_random(Result).



random_view_doc(Db, DDoc, ViewName) ->
    %% get number of docs in the view
    {ok, [{meta, Meta}]} = couch_mrview:query_view(Db, DDoc, ViewName,
        [{limit, 0}]),
    %% generate random value
    N = case couch_util:get_value(total, Meta) of
        C when C < 1 -> C;
        C -> crypto:rand_uniform(0, C)
    end,
    Args = #mrargs{skip=N, limit=1, include_docs=true},
    {ok, Acc} = couch_mrview:query_view(Db, DDoc, ViewName,
        Args, fun view_cb/2, nil),
    finish_random(Acc).

view_cb({row, Row}, _Acc) ->
    Doc = couch_util:get_value(doc, Row),
    {ok, couch_doc:from_json_obj(Doc)};
view_cb(_Other, Acc) ->
    {ok, Acc}.

finish_random(Result) ->
    case Result of
        #doc{} ->
            {ok, Result};
        _ ->
           nil
    end.
