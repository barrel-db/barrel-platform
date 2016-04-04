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



-module(couch_randomdoc_httpd).

-export([handle_req/2, parse_query/1, make_filter/3, get_random_doc/3]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_randomdoc.hrl").

handle_req(#httpd{method='GET'}=Req, Db) ->
    #random_query{options = Opts,
                  filter = FilterName} =parse_query(Req),

    JsonObj = case get_random_doc(Req, Db, FilterName) of
        {ok, Doc} ->
            couch_doc:to_json_obj(Doc, Opts);
        nil ->
            null
    end,
    couch_httpd:send_json(Req, 200, JsonObj);

handle_req(Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


get_random_doc(Req, Db, FilterName) ->
    case FilterName of
        <<"_view">> ->
            ViewSpec = list_to_binary(couch_httpd:qs_value(Req, "view", "")),
            case binary:split(ViewSpec, <<"/">>) of
            [DName, VName] ->
                DesignId = <<"_design/", DName/binary>>,
                DDoc = couch_httpd_db:couch_doc_open(Db, DesignId, nil, [ejson_body]),
                couch_randomdoc:random_view_doc(Db, DDoc, VName);
            _ ->
                 throw({bad_request, "view name should be on the form DName/FName"})
            end;
        _ ->
            FilterFun = make_filter(FilterName, Req, Db),
            couch_randomdoc:random_doc(Db, FilterFun)
    end.


%% internal
parse_query(Req) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"attachments", "true"} ->
            Options = [attachments | Args#random_query.options],
            Args#random_query{options=Options};
        {"meta", "true"} ->
            Options = [revs_info, conflicts, deleted_conflicts | Args#random_query.options],
            Args#random_query{options=Options};
        {"revs", "true"} ->
            Options = [revs | Args#random_query.options],
            Args#random_query{options=Options};
        {"local_seq", "true"} ->
            Options = [local_seq | Args#random_query.options],
            Args#random_query{options=Options};
        {"revs_info", "true"} ->
            Options = [revs_info | Args#random_query.options],
            Args#random_query{options=Options};
        {"conflicts", "true"} ->
            Options = [conflicts | Args#random_query.options],
            Args#random_query{options=Options};
        {"deleted_conflicts", "true"} ->
            Options = [deleted_conflicts | Args#random_query.options],
            Args#random_query{options=Options};
        {"latest", "true"} ->
            Options = [latest | Args#random_query.options],
            Args#random_query{options=Options};
        {"att_encoding_info", "true"} ->
            Options = [att_encoding_info | Args#random_query.options],
            Args#random_query{options=Options};
        {"filter", Filter} ->
            Args#random_query{filter=list_to_binary(mochiweb_util:unquote(Filter))};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #random_query{}, couch_httpd:qs(Req)).


make_filter(nil, _Req, _Db) ->
    fun(_Db1, _Doc) -> true end;
make_filter(<<"_", _/binary>> = FilterName, Req, _Db) ->
    builtin_filter(FilterName, Req);
make_filter(FilterName, Req, Db) ->
    os_filter_fun(FilterName, Req, Db).


builtin_filter(<<"_prefix">>, Req) ->
    Prefix = list_to_binary(couch_httpd:qs_value(Req, "prefix", "")),
    filter_prefix(Prefix);
builtin_filter(<<"_design_doc">>, _Req) ->
    filter_ddoc();
builtin_filter(_FilterName, _Req) ->
    throw({bad_request, "unknown builtin filter name"}).

filter_prefix(Prefix) ->
    S = size(Prefix),
    fun(_Db, #doc{id = DocId}) ->
        case DocId of
            <<Prefix:S/binary, _/binary>> ->
                true;
            _ ->
                false
        end
    end.

filter_ddoc() ->
    fun(_Db, DocInfo) ->
        case DocInfo of
        #doc{id = <<"_design/", _/binary>>} ->
            true;
        _ ->
            false
        end
    end.

os_filter_fun(FilterName, Req, Db) ->
    case binary:split(FilterName, <<"/">>) of
        [DName, FName] ->
            DesignId = <<"_design/", DName/binary>>,
            DDoc = couch_httpd_db:couch_doc_open(Db, DesignId, nil, [ejson_body]),
            % validate that the ddoc has the filter fun
            #doc{body={Props}} = DDoc,
            couch_util:get_nested_json_value({Props}, [<<"filters">>, FName]),
            fun(Db2, Doc) ->
                {ok, Passes} = couch_query_servers:filter_docs(
                    Req, Db2, DDoc, FName, [Doc]
                ),
                lists:member(true, Passes)
            end;
        _ ->
            throw({bad_request, "filter name should be on the form DName/FName"})
    end.


