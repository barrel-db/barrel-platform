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

-include_lib("couch_db.hrl").
-include("couch_randomdoc.hrl").


handle_randomdoc_show_req(#httpd{
            path_parts=[_, _, _, _, ShowName]
        }=Req, Db, DDoc) ->

    #random_query{options = Opts,
                  filter=FilterName} = couch_randomdoc_httpd:parse_query(Req),

    {JsonDoc, DocId} =
    case couch_randomdoc_httpd:get_random_doc(Req, Db, FilterName) of
        {ok, #doc{id=Id}=Doc} ->
            {barrel_doc:to_json_obj(Doc, Opts), Id};
        nil ->
            {null, nil}
    end,

    CurrentEtag = barrel_uuids:new(),
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


apply_etag(ExternalResponse, CurrentEtag) ->
    % Here we embark on the delicate task of replacing or creating the
    % headers on the JsonResponse object. We need to control the Etag and
    % Vary headers. If the external function controls the Etag, we'd have to
    % run it to check for a match, which sort of defeats the purpose.
    case maps:find(<<"headers">>, ExternalResponse) of
        error ->
            % no JSON headers
            % add our Etag and Vary headers to the response
            ExternalResponse#{<<"headers">> => #{ <<"Etag">> => CurrentEtag, <<"Vary">> =>  <<"Accept">>}};
        {ok, JsonHeaders} ->
            JsonHeaders2 = JsonHeaders#{<<"Etag">> => CurrentEtag, <<"Vary">> =>  <<"Accept">>},
            ExternalResponse#{<<"headers">> => JsonHeaders2
    end.
