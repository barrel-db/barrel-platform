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



-module(couch_randomdoc_show).

-export([handle_randomdoc_show_req/3]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_randomdoc.hrl").


handle_randomdoc_show_req(#httpd{
            path_parts=[_, _, _, _, ShowName]
        }=Req, Db, DDoc) ->

    #random_query{options = Opts,
                  filter=FilterName} = couch_randomdoc_httpd:parse_query(Req),

    {JsonDoc, DocId} =
    case couch_randomdoc_httpd:get_random_doc(Req, Db, FilterName) of
        {ok, #doc{id=Id}=Doc} ->
            {couch_doc:to_json_obj(Doc, Opts), Id};
        nil ->
            {null, nil}
    end,

    CurrentEtag = couch_uuids:new(),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() ->
        JsonReq = couch_httpd_external:json_req_obj(Req, Db, DocId),
        [<<"resp">>, ExternalResp] =
            couch_query_servers:ddoc_prompt(DDoc, [<<"shows">>, ShowName],
                [JsonDoc, JsonReq]),
        JsonResp = apply_etag(ExternalResp, CurrentEtag),
        couch_httpd_external:send_external_response(Req, JsonResp)
    end);

handle_randomdoc_show_req(Req, _Db, _DDoc) ->
    couch_httpd:send_error(Req, 404, <<"show_error">>, <<"Invalid path.">>).


apply_etag({ExternalResponse}, CurrentEtag) ->
    % Here we embark on the delicate task of replacing or creating the
    % headers on the JsonResponse object. We need to control the Etag and
    % Vary headers. If the external function controls the Etag, we'd have to
    % run it to check for a match, which sort of defeats the purpose.
    case couch_util:get_value(<<"headers">>, ExternalResponse, nil) of
    nil ->
        % no JSON headers
        % add our Etag and Vary headers to the response
        {[{<<"headers">>, {[{<<"Etag">>, CurrentEtag}, {<<"Vary">>, <<"Accept">>}]}} | ExternalResponse]};
    JsonHeaders ->
        {[case Field of
        {<<"headers">>, JsonHeaders} -> % add our headers
            JsonHeadersEtagged = json_apply_field({<<"Etag">>, CurrentEtag}, JsonHeaders),
            JsonHeadersVaried = json_apply_field({<<"Vary">>, <<"Accept">>}, JsonHeadersEtagged),
            {<<"headers">>, JsonHeadersVaried};
        _ -> % skip non-header fields
            Field
        end || Field <- ExternalResponse]}
    end.


% Maybe this is in the proplists API
% todo move to couch_util
json_apply_field(H, {L}) ->
    json_apply_field(H, L, []).


json_apply_field({Key, NewValue}, [{Key, _OldVal} | Headers], Acc) ->
    % drop matching keys
    json_apply_field({Key, NewValue}, Headers, Acc);
json_apply_field({Key, NewValue}, [{OtherKey, OtherVal} | Headers], Acc) ->
    % something else is next, leave it alone.
    json_apply_field({Key, NewValue}, Headers, [{OtherKey, OtherVal} | Acc]);
json_apply_field({Key, NewValue}, [], Acc) ->
    % end of list, add ours
    {[{Key, NewValue}|Acc]}.

