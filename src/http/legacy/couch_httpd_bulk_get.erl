%% Copyright 2016, Benoit Chesneau
%%
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

-include("db.hrl").
-include("couch_httpd.hrl").

-export([handle_req/2]).

handle_req(#httpd{method='POST',path_parts=[_,<<"_bulk_get">>],
                  mochi_req=MochiReq}=Req, Db) ->
  couch_httpd:validate_ctype(Req, "application/json"),
  JsonProps = couch_httpd:json_body_obj(Req),
  case maps:get(<<"docs">>, JsonProps) of
    undefined ->
      couch_httpd:send_error(Req, 400, <<"bad_request">>, <<"Missing JSON list of 'docs'.">>);
    Docs ->
      #doc_query_args{
         options = Options
        } = couch_httpd_db:parse_doc_query(Req),

      %% start the response
      AcceptMixedMp = MochiReq:accepts_content_type("multipart/mixed"),
      AcceptRelatedMp = MochiReq:accepts_content_type("multipart/related"),

      %% do we accept multipart
      if
        AcceptMixedMp orelse AcceptRelatedMp -> send_multipart_response(Req, Db, Docs, Options, AcceptMixedMp);
        true -> send_json_response(Req, Db, Docs, Options)
      end
  end;
handle_req(#httpd{path_parts=[_,<<"_bulk_get">>]}=Req, _Db) ->
  couch_httpd:send_method_not_allowed(Req, "POST").

%%%
%%% JSON RESPONSE compatible with pouchdb
%%%



send_json_response(Req, Db, Docs, Options) ->
  {ok, Resp} = couch_httpd:start_json_response(Req, 200),
  couch_httpd:send_chunk(Resp, "{\"results\": ["),

  lists:foldr(fun(Obj, Sep) ->
                  {DocId, Results, Options1} = json_open_doc_revs(Db, Obj, Options),
                  send_docs_json(Resp, DocId, Results, Options1, Sep),
                  <<",">>

              end, "", Docs),
  couch_httpd:send_chunk(Resp, <<"]}">>),
  couch_httpd:end_json_response(Resp).


send_docs_json(Resp, DocId, Results, Options, Sep) ->
  Id = ?JSON_ENCODE(DocId),
  couch_httpd:send_chunk(Resp, [Sep, <<"{\"id\": ">>, Id, <<", \"docs\": [">>]),
  send_docs_json1(Resp, DocId, Results, Options),
  couch_httpd:send_chunk(Resp, <<"]}">>).

send_docs_json1(Resp, DocId, {error, {Rev, Error, Reason}}, _) ->
  couch_httpd:send_chunk(Resp, [bulk_get_json_error(DocId, Rev, Error, Reason)]);
send_docs_json1(_Resp, _DocId, {ok, []}, _) ->
  ok;
send_docs_json1(Resp, DocId, {ok, Docs}, Options) ->
  lists:foldl(fun(Result, AccSeparator) ->
                  case Result of
                    {ok, Doc} ->
                      JsonDoc = barrel_doc:to_json_obj(Doc, Options),
                      Json = ?JSON_ENCODE(#{ok => JsonDoc}),
                      couch_httpd:send_chunk(Resp, [AccSeparator, Json]);
                    {{Error, Reason}, RevId} ->
                      RevStr = barrel_doc:rev_to_str(RevId),
                      Json = bulk_get_json_error(DocId, RevStr, Error, Reason),
                      couch_httpd:send_chunk(Resp, [AccSeparator, Json])
                  end,
                  <<",">>
              end, <<"">>, Docs).

bulk_get_json_error(DocId, Rev, Error, Reason) ->
  ?JSON_ENCODE(#{error => #{<<"id">> => DocId,
                            <<"rev">> => Rev,
                            <<"error">> => barrel:to_error(Error),
                            <<"reason">> => barrel:to_error(Reason)}}).

json_open_doc_revs(Db, Props, Options) ->
  json_open_doc_revs1(Db, Props, Options, {}).


json_open_doc_revs1(Db, Props, Options, {}) ->
  case parse_field(<<"id">>, maps:get(<<"id">>, Props, undefined)) of
    {error, {DocId, Error, Reason}} ->
      {DocId, {error, {null, Error, Reason}}, Options};

    {ok, undefined} ->
      Error = {null, bad_request, <<"document id missed">>},
      {null, {error, Error}, Options};

    {ok, DocId} ->
      json_open_doc_revs1(Db, Props, Options, {DocId})
  end;
json_open_doc_revs1(Db, Props, Options, {DocId}) ->
  RevStr = maps:get(<<"rev">>, Props, undefined),

  case parse_field(<<"rev">>, RevStr) of
    {error, {RevStr, Error, Reason}} ->
      {DocId, {error, {RevStr, Error, Reason}}, Options};

    {ok, undefined} ->
      json_open_doc_revs1(Db, Props, Options, {DocId, all});

    {ok, Rev} ->
      json_open_doc_revs1(Db, Props, Options, {DocId, [Rev]})
  end;
json_open_doc_revs1(Db, Props, Options, {DocId, Revs}) ->
  AttsSinceStr = maps:get(<<"atts_since">>, Props, undefined),

  case parse_field(<<"atts_since">>, AttsSinceStr) of
    {error, {BadAttsSinceRev, Error, Reason}} ->
      {DocId, {error, {BadAttsSinceRev, Error, Reason}}, Options};

    {ok, []} ->
      json_open_doc_revs1(Db, Props, Options, {DocId, Revs, Options});

    {ok, RevList} ->
      Options1 = [{atts_since, RevList}, attachments | Options],
      json_open_doc_revs1(Db, Props, Options, {DocId, Revs, Options1})
  end;
json_open_doc_revs1(Db, Props, _, {DocId, Revs, Options}) ->
  case couch_db:open_doc_revs(Db, DocId, Revs, Options) of
    {ok, []} ->
      RevStr = maps:get(<<"rev">>, Props, undefined),
      Error = {RevStr, <<"not_found">>, <<"missing">>},
      {DocId, {error, Error}, Options};
    Results ->
      {DocId, Results, Options}
  end.

parse_field(<<"id">>, undefined) ->
  {ok, undefined};
parse_field(<<"id">>, Value) ->
  try
    ok = barrel_doc:validate_docid(Value),
    {ok, Value}
  catch
    throw:{Error, Reason} ->
      {error, {Value, Error, Reason}}
  end;
parse_field(<<"rev">>, undefined) ->
  {ok, undefined};
parse_field(<<"rev">>, Value) ->
  try
    Rev = barrel_doc:parse_rev(Value),
    {ok, Rev}
  catch
    throw:{bad_request=Error, Reason} ->
      {error, {Value, Error, Reason}}
  end;
parse_field(<<"atts_since">>, undefined) ->
  {ok, []};
parse_field(<<"atts_since">>, []) ->
  {ok, []};
parse_field(<<"atts_since">>, Value) when is_list(Value) ->
  parse_atts_since(Value, []);
parse_field(<<"atts_since">>, Value) ->
  {error, {Value, bad_request, <<"att_since value must be array of revs.">>}}.


parse_atts_since([], Acc) ->
  {ok, lists:reverse(Acc)};
parse_atts_since([RevStr | Rest], Acc) ->
  case parse_field(<<"rev">>, RevStr) of
    {ok, Rev} ->
      parse_atts_since(Rest, [Rev | Acc]);
    {error, _}=Error ->
      Error
  end.

%%%
%%% MULTIPART RESPONSE
%%%


send_multipart_response(Req, Db, Docs, Options, AcceptMixedMp) ->
  Boundary = hackney_multipart:boundary(),

  %% some versions of couchbase-lite only accept
  %% multipart/related at top level.
  %% https://github.com/couchbase/couchbase-lite-ios/issues/255
  MpType = case AcceptMixedMp of
             true -> "multipart/mixed";
             _ ->  "multipart/related"
           end,

  CType = {"Content-Type", MpType ++ "; boundary=\"" ++
           binary_to_list(Boundary) ++  "\""},
  {ok, Resp} = couch_httpd:start_chunked_response(Req, 200, [CType]),
  lists:foldl(fun(Obj, Pre) ->
                  {DocId, Results, Options1} = open_doc_revs(Obj, Db, Options),
                  case Results of
                    [] -> Pre;
                    _ -> send_docs_multipart(Resp, Pre, DocId,
                                             Results, Boundary,
                                             Options1)
                  end
              end, <<"">>, Docs),
  %% send the end of the multipart if needed
  case Docs of
    [] -> ok;
    _Else ->
      Eof = hackney_multipart:mp_eof(Boundary),
      couch_httpd:send_chunk(Resp, <<"\r\n", Eof/binary >>)
  end,
  couch_httpd:last_chunk(Resp).




send_docs_multipart(Resp, Pre, DocId, Results, OuterBoundary, Options0) ->
  Options = [attachments, follows, att_encoding_info | Options0],

  lists:foldl(fun
                ({ok, #doc{atts=[]}=Doc}, Pre1) ->
                  JsonBytes = ?JSON_ENCODE(barrel_doc:to_json_obj(Doc, Options)),
                  Headers = [{<<"Content-Type">>, <<"application/json">>}],
                  Part = part(JsonBytes, Headers,
                              OuterBoundary),
                  couch_httpd:send_chunk(Resp, << Pre1/binary, Part/binary >>),
                  <<"\r\n">>;
                ({ok, #doc{id=Id, revs=Revs, atts=Atts}=Doc}, Pre1) ->
                  %% create inner binary
                  InnerBoundary = hackney_multipart:boundary(),

                  %% start the related part, we first send the json
                  JsonBytes = ?JSON_ENCODE(barrel_doc:to_json_obj(Doc, Options)),
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
                  RevStr = barrel_doc:rev_to_str(RevId),
                  Body = #{<<"id">> => DocId,
                           <<"error">> => <<"not_found">>,
                           <<"reason">> => <<"missing">>,
                           <<"status">> => 400,
                           <<"missing">> => RevStr},
                  Json = ?JSON_ENCODE(Body),

                  Headers = [{<<"Content-Type">>, <<"application/json">>}],
                  Part = part(Json, Headers, OuterBoundary),
                  couch_httpd:send_chunk(Resp, <<Pre1/binary, Part/binary >>),
                  <<"\r\n">>
              end, Pre, Results).



open_doc_revs(Props, Db, Options) ->
  DocId = maps:get(<<"id">>, Props),
  Revs = case maps:get(<<"rev">>, Props, undefined) of
           undefined -> all;
           Rev -> barrel_doc:parse_revs([binary_to_list(Rev)])
         end,
  Options1 = case maps:get(<<"atts_since">>, Props, []) of
               [] -> Options;
               RevList when is_list(RevList) ->
                 RevList1 = barrel_doc:parse_revs(RevList),
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
  RevStr = barrel_doc:rev_to_str({Start, FirstRevId}),
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
