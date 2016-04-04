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
%
-module(couch_httpd_bulk_get).

-include_lib("couch/include/couch_db.hrl").
-include("couch_httpd.hrl").

-export([handle_req/2]).

handle_req(#httpd{method='POST',path_parts=[_,<<"_bulk_get">>],
              mochi_req=MochiReq}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    couch_httpd:validate_ctype(Req, "application/json"),
    {JsonProps} = couch_httpd:json_body_obj(Req),
    case couch_util:get_value(<<"docs">>, JsonProps) of
       undefined ->
            couch_httpd:send_error(Req, 400,
                       <<"bad_request">>, <<"Missing JSON list of
                                          'docs'.">>);
        DocsArray ->
            #doc_query_args{
                options = Options
            } = couch_httpd_db:parse_doc_query(Req),

            %% start the response
            AcceptMixedMp = MochiReq:accepts_content_type("multipart/mixed"),
            AcceptRelatedMp = MochiReq:accepts_content_type("multipart/related"),

            %% do we accept multipart
            AcceptMp = AcceptMixedMp orelse AcceptRelatedMp,

            {Resp, Boundary} = case AcceptMp of
                false ->
                    {ok, Resp1} = couch_httpd:start_json_response(Req, 200),
                    couch_httpd:send_chunk(Resp1, "{\"results\": ["),
                    {Resp1, nil};
                true ->
                    Boundary1 = hackney_multipart:boundary(),

                    %% some versions of couchbase-lite only accept
                    %% multipart/related at top level.
                    %% https://github.com/couchbase/couchbase-lite-ios/issues/255
                    MpType = case AcceptMixedMp of
                        true -> "multipart/mixed";
                        _ ->  "multipart/related"
                    end,

                    CType = {"Content-Type", MpType ++ "; boundary=\"" ++
                             binary_to_list(Boundary1) ++  "\""},
                    {ok, Resp1} = couch_httpd:start_chunked_response(Req, 200,
                                                                     [CType]),
                    {Resp1, Boundary1}
            end,


            case Boundary of
                nil ->
                    lists:foldr(fun(Obj, Sep) ->
                            {DocId, Results, Options1} = open_doc_revs(Obj, Db, Options),
                            case Results of
                                [] -> Sep;
                                _ ->
                                    send_docs(Resp, DocId, Results, Options1,
                                              Sep),
                                    ","
                            end
                        end, "", DocsArray),
                        couch_httpd:send_chunk(Resp1, <<"]}">>),
                        couch_httpd:end_json_response(Resp);
                _ ->
                    lists:foldl(fun(Obj, Pre) ->
                                {DocId, Results, Options1} = open_doc_revs(Obj, Db, Options),
                                case Results of
                                    [] -> Pre;
                                    _ -> send_docs_multipart(Resp, Pre, DocId,
                                                             Results, Boundary,
                                                             Options1)
                                end
                        end, <<"">>, DocsArray),
                    %% send the end of the multipart if needed
                    case DocsArray of
                        [] -> ok;
                        _Else ->
                            Eof = hackney_multipart:mp_eof(Boundary),
                            couch_httpd:send_chunk(Resp, <<"\r\n", Eof/binary >>)
                    end,
                    couch_httpd:last_chunk(Resp)
            end
    end;
handle_req(#httpd{path_parts=[_,<<"_bulk_get">>]}=Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "POST").

send_docs(Resp, DocId, Results, Options, Sep) ->
    couch_httpd:send_chunk(Resp, [Sep, "{ \"id\": \"",
                                  ?JSON_ENCODE(DocId), "\", \"docs\": ["]),
    lists:foldl(
        fun(Result, AccSeparator) ->
                case Result of
                    {ok, Doc} ->
                        JsonDoc = couch_doc:to_json_obj(Doc, Options),
                        Json = ?JSON_ENCODE({[{ok, JsonDoc}]}),
                        couch_httpd:send_chunk(Resp, AccSeparator ++ Json);
                    {{not_found, missing}, RevId} ->
                        RevStr = couch_doc:rev_to_str(RevId),
                        Json = ?JSON_ENCODE({[{"missing", RevStr}]}),
                        couch_httpd:send_chunk(Resp, AccSeparator ++ Json)
                end,
                "," % AccSeparator now has a comma
        end, "", Results),
    couch_httpd:send_chunk(Resp, "]}").


send_docs_multipart(Resp, Pre, DocId, Results, OuterBoundary, Options0) ->
    Options = [attachments, follows, att_encoding_info | Options0],

        lists:foldl(fun
            ({ok, #doc{atts=[]}=Doc}, Pre1) ->
                JsonBytes = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, Options)),
                Headers = [{<<"Content-Type">>, <<"application/json">>}],
                Part = part(JsonBytes, Headers,
                                              OuterBoundary),
                couch_httpd:send_chunk(Resp, << Pre1/binary, Part/binary >>),
                <<"\r\n">>;
            ({ok, #doc{id=Id, revs=Revs, atts=Atts}=Doc}, Pre1) ->
                %% create inner binary
                InnerBoundary = hackney_multipart:boundary(),

                %% start the related part, we first send the json
                JsonBytes = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, Options)),
                Headers = mp_header(Revs, Id, InnerBoundary),
                BinHeaders = hackney_headers:to_binary(Headers),
                Bin = <<Pre1/binary, "--", OuterBoundary/binary, "\r\n", BinHeaders/binary >>,
                couch_httpd:send_chunk(Resp, Bin),

                %% send doc part
                JsonHeaders = [{<<"Content-Type">>, <<"application/json">>}],
                DocPart = part(JsonBytes, JsonHeaders,
                                                 InnerBoundary),
                couch_httpd:send_chunk(Resp, DocPart),

                %% send attachments
                {ok, _} = atts_to_mp(Atts, InnerBoundary,
                                fun(Data) ->
                                        couch_httpd:send_chunk(Resp, Data)
                                end),
                <<"\r\n">>;
            ({{not_found, missing}, RevId}, Pre1) ->
                RevStr = couch_doc:rev_to_str(RevId),
                Body = {[{<<"id">>, DocId},
                         {<<"error">>, <<"not_found">>},
                         {<<"reason">>, <<"missing">>},
                         {<<"status">>, 400},
                         {<<"missing">>, RevStr}]},
                Json = ?JSON_ENCODE(Body),

                Headers = [{<<"Content-Type">>, <<"application/json">>}],
                Part = part(Json, Headers, OuterBoundary),
                couch_httpd:send_chunk(Resp, <<Pre1/binary, Part/binary >>),
                 <<"\r\n">>
        end, Pre, Results).



open_doc_revs({Props}, Db, Options) ->
    DocId = couch_util:get_value(<<"id">>, Props),
    Revs = case couch_util:get_value(<<"rev">>, Props) of
               undefined -> all;
               Rev -> couch_doc:parse_revs([binary_to_list(Rev)])
           end,
    Options1 = case couch_util:get_value(<<"atts_since">>, Props, []) of
                   [] -> Options;
                   RevList when is_list(RevList) ->
                       RevList1 = couch_doc:parse_revs(RevList),
                       [{atts_since, RevList1}, attachments |Options]
               end,

    %% get doc informations
    {ok, Results} = couch_db:open_doc_revs(Db, DocId, Revs, Options),
    {DocId, Results, Options1}.

part(Content, Headers, Boundary) ->
    BinHeaders = hackney_headers:to_binary(Headers),
    <<"--", Boundary/binary, "\r\n", BinHeaders/binary, Content/binary >>.


mp_header({0, []}, Id, Boundary) ->
    [{<<"X-Doc-Id">>, Id},
     {<<"Content-Type">>, <<"multipart/related; boundary=",
                            Boundary/binary >>}];
mp_header({Start, [FirstRevId|_]}, Id, Boundary) ->
    RevStr = couch_doc:rev_to_str({Start, FirstRevId}),
    [{<<"X-Doc-Id">>, Id},
     {<<"X-Rev-Id">>, RevStr},
     {<<"Content-Type">>, <<"multipart/related; boundary=",
                            Boundary/binary >>}].


atts_to_mp([], Boundary, WriteFun) ->
    Eof = hackney_multipart:mp_eof(Boundary),
    WriteFun(<<"\r\n", Eof/binary>>);
atts_to_mp([#att{data=stub} | RestAtts], Boundary, WriteFun) ->
    atts_to_mp(RestAtts, Boundary, WriteFun);
atts_to_mp([Att | RestAtts], Boundary, WriteFun)  ->
    #att{
        name=Name,
        att_len=AttLen,
        type=Type,
        encoding=Encoding
    } = Att,

    % write headers
    LengthBin = list_to_binary(integer_to_list(AttLen)),
    WriteFun(<<"\r\n--", Boundary/binary>>),
    WriteFun(<<"\r\nContent-Disposition: attachment; filename=\"", Name/binary, "\"">>),
    WriteFun(<<"\r\nContent-Type: ", Type/binary>>),
    WriteFun(<<"\r\nContent-Length: ", LengthBin/binary>>),
    case Encoding of
        identity ->
            ok;
        _ ->
            EncodingBin = atom_to_binary(Encoding, latin1),
            WriteFun(<<"\r\nContent-Encoding: ", EncodingBin/binary>>)
    end,

    % write data
    WriteFun(<<"\r\n\r\n">>),
    att_foldl(Att, fun(Data, _) -> WriteFun(Data) end, ok),
    atts_to_mp(RestAtts, Boundary, WriteFun).

att_foldl(#att{data=Bin}, Fun, Acc) when is_binary(Bin) ->
    Fun(Bin, Acc);
att_foldl(#att{data={Fd,Sp},md5=Md5}, Fun, Acc) ->
    couch_stream:foldl(Fd, Sp, Md5, Fun, Acc);
att_foldl(#att{data=DataFun,att_len=Len}, Fun, Acc) when is_function(DataFun) ->
   fold_streamed_data(DataFun, Len, Fun, Acc).

fold_streamed_data(_RcvFun, 0, _Fun, Acc) ->
    Acc;
fold_streamed_data(RcvFun, LenLeft, Fun, Acc) when LenLeft > 0->
    Bin = RcvFun(),
    ResultAcc = Fun(Bin, Acc),
    fold_streamed_data(RcvFun, LenLeft - size(Bin), Fun, ResultAcc).
