%% Copyright 2016, Benoit Chesneau
%% Copyright 2009-2014 The Apache Software Foundation
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

-module(barrel_doc).

-export([to_doc_info/1,to_doc_info_path/1,parse_rev/1,parse_revs/1,rev_to_str/1,revs_to_strs/1]).
-export([att_foldl/3,range_att_foldl/5,att_foldl_decode/3,
         get_validate_doc_fun/1,get_validate_read_doc_fun/1]).
-export([from_json_obj/1,to_json_obj/2,has_stubs/1, merge_stubs/2]).
-export([validate_docid/1]).
-export([doc_from_multi_part_stream/2]).
-export([doc_to_multi_part_stream/5, doc_to_multi_part_stream/6,
         len_doc_to_multi_part_stream/4, len_doc_to_multi_part_stream/5]).
-export([abort_multi_part_stream/1]).
-export([to_path/1]).
-export([mp_parse_doc/2]).
-export([with_ejson_body/1]).
-export([load/3]).

-include("couch_db.hrl").

-spec to_path(#doc{}) -> path().
to_path(#doc{revs={Start, RevIds}}=Doc) ->
    [Branch] = to_branch(Doc, lists:reverse(RevIds)),
    {Start - length(RevIds) + 1, Branch}.

-spec to_branch(#doc{}, [RevId::binary()]) -> [branch()].
to_branch(Doc, [RevId]) ->
    [{RevId, Doc, []}];
to_branch(Doc, [RevId | Rest]) ->
    [{RevId, ?REV_MISSING, to_branch(Doc, Rest)}].

% helpers used by to_json_obj
to_json_rev(0, [], Body) -> Body;
to_json_rev(Start, [FirstRevId|_], Body) ->
    Body#{<<"_rev">> => rev_to_str({Start, FirstRevId})}.

to_json_body(true, Body) -> Body#{<<"_deleted">> => true};
to_json_body(removed, Body) -> Body#{<<"_removed">> => true}; %% only for view changes
to_json_body(false, Body) -> Body.

to_json_revisions(Options, Start, RevIds, Body) ->
    case lists:member(revs, Options) of
        false -> Body;
        true ->
            Revisions = #{ <<"start">> => Start,
                           <<"ids">> => [revid_to_str(R) || R <- RevIds]
                        },
            Body#{<<"_revisions">> => Revisions}
    end.

revid_to_str(RevId) when size(RevId) =:= 16 -> list_to_binary(couch_util:to_hex(RevId));
revid_to_str(RevId) -> RevId.

rev_to_str({Pos, RevId}) ->
    list_to_binary([integer_to_list(Pos),"-",revid_to_str(RevId)]).

revs_to_strs([]) -> [];
revs_to_strs([{Pos, RevId}| Rest]) -> [rev_to_str({Pos, RevId}) | revs_to_strs(Rest)].

to_json_meta(Meta, Body) ->
    lists:foldl(
        fun({revs_info, Start, RevsInfo}, Body1) ->
            {JsonRevsInfo, _Pos}  = lists:mapfoldl(
                fun({RevId, Status}, PosAcc) ->
                    JsonObj = #{<<"rev">> => rev_to_str({PosAcc, RevId}),
                                <<"status">> => list_to_binary(atom_to_list(Status))},
                    {JsonObj, PosAcc - 1}
                end, Start, RevsInfo),
            Body1#{<<"_revs_info">> => JsonRevsInfo};
        ({local_seq, Seq}, Body1) ->
            Body1#{<<"_local_seq">> => Seq};
        ({conflicts, Conflicts}, Body1) ->
            Body1#{<<"_conflicts">> => revs_to_strs(Conflicts)};
        ({deleted_conflicts, DConflicts}, Body1) ->
            Body1#{<<"_deleted_conflicts">> => revs_to_strs(DConflicts)}
        end, Body, Meta).

%% TODO: optimize the way we retrieve an attachements map
to_json_attachments(Attachments, Options, Body) ->
    to_json_attachments(
        Attachments,
        lists:member(attachments, Options),
        lists:member(follows, Options),
        lists:member(att_encoding_info, Options),
        Body
    ).

to_json_attachments([], _OutputData, _DataToFollow, _ShowEncInfo, Body) ->
    Body;
to_json_attachments(Atts, OutputData, DataToFollow, ShowEncInfo, Body) ->
    AttProps = lists:map(
        fun(#att{disk_len=DiskLen, att_len=AttLen, encoding=Enc}=Att) ->
            {Att#att.name, maps:from_list([
                {<<"content_type">>, Att#att.type},
                {<<"revpos">>, Att#att.revpos}] ++
                case Att#att.md5 of
                    <<>> ->
                        [];
                    Md5 ->
                        EncodedMd5 = base64:encode(Md5),
                        [{<<"digest">>, <<"md5-",EncodedMd5/binary>>}]
                end ++
                if not OutputData orelse Att#att.data == stub ->
                    [{<<"length">>, DiskLen}, {<<"stub">>, true}];
                true ->
                    if DataToFollow ->
                        [{<<"length">>, DiskLen}, {<<"follows">>, true}];
                    true ->
                        AttData = case Enc of
                        gzip ->
                            zlib:gunzip(att_to_bin(Att));
                        identity ->
                            att_to_bin(Att)
                        end,
                        [{<<"data">>, base64:encode(AttData)}]
                    end
                end ++
                    case {ShowEncInfo, Enc} of
                    {false, _} ->
                        [];
                    {true, identity} ->
                        [];
                    {true, _} ->
                        [
                            {<<"encoding">>, couch_util:to_binary(Enc)},
                            {<<"encoded_length">>, AttLen}
                        ]
                    end
            )}
        end, Atts),
    Body#{<<"_attachments">> => maps:from_list(AttProps)}.

to_json_obj(Doc, Options) ->
    doc_to_json_obj(with_ejson_body(Doc), Options).

doc_to_json_obj(#doc{id=Id,deleted=Del,body=Body,revs={Start, RevIds},
            meta=Meta}=Doc,Options)->

    to_json_attachments(Doc#doc.atts, Options,
        to_json_meta(Meta,
            to_json_revisions(Options, Start, RevIds,
                to_json_rev(Start, RevIds,
                    to_json_body(Del, Body#{ <<"_id">> => Id}))))).


from_json_obj(Obj) when is_map(Obj) -> transfer_fields(Obj, #doc{body=#{}});
from_json_obj(_Other) -> throw({bad_request, "Document must be a JSON object"}).

parse_revid(RevId) when size(RevId) =:= 32 ->
    RevInt = erlang:list_to_integer(binary_to_list(RevId), 16),
     <<RevInt:128>>;
parse_revid(RevId) when length(RevId) =:= 32 ->
    RevInt = erlang:list_to_integer(RevId, 16),
     <<RevInt:128>>;
parse_revid(RevId) when is_binary(RevId) ->
    RevId;
parse_revid(RevId) when is_list(RevId) ->
    list_to_binary(RevId).


parse_rev(Rev) when is_binary(Rev) ->
    parse_rev(binary_to_list(Rev));
parse_rev(Rev) when is_list(Rev) ->
    SplitRev = lists:splitwith(fun($-) -> false; (_) -> true end, Rev),
    case SplitRev of
        {Pos, [$- | RevId]} -> {list_to_integer(Pos), parse_revid(RevId)};
        _Else -> throw({bad_request, <<"Invalid rev format">>})
    end;
parse_rev(_BadRev) ->
    throw({bad_request, <<"Invalid rev format">>}).

parse_revs([]) ->
    [];
parse_revs([Rev | Rest]) ->
    [parse_rev(Rev) | parse_revs(Rest)].


validate_docid(<<"">>) ->
    throw({bad_request, <<"Document id must not be empty">>});
validate_docid(Id) when is_binary(Id) ->
    case couch_util:validate_utf8(Id) of
        false -> throw({bad_request, <<"Document id must be valid UTF-8">>});
        true -> ok
    end,
    case Id of
    <<"_design/", _/binary>> -> ok;
    <<"_local/", _/binary>> -> ok;
    <<"_", _/binary>> ->
        throw({bad_request, <<"Only reserved document ids may start with underscore.">>});
    _Else -> ok
    end;
validate_docid(Id) ->
    lager:debug("Document id is not a string: ~p", [Id]),
    throw({bad_request, <<"Document id must be a string">>}).


transfer_fields(Obj, Doc) ->
    maps:fold(fun transfer_fields1/3, Doc, Obj).


transfer_fields1(<<"_id">>, Id, Doc) ->
    validate_docid(Id),
    Doc#doc{id=Id};

transfer_fields1(<<"_rev">>, Rev, #doc{revs={0, []}}=Doc) ->
    {Pos, RevId} = parse_rev(Rev),
    Doc#doc{revs={Pos, [RevId]}};

transfer_fields1(<<"_rev">>, _Rev, Doc) ->
    Doc;

transfer_fields1(<<"_attachments">>, JsonBins, Doc) ->
    Atts =  maps:fold(fun(Name, BinProps, Atts1) ->
        Md5 = case BinProps of
            #{ <<"digest">> := <<"md5-",EncodedMd5/binary>>} -> base64:decode(EncodedMd5);
            _ -> <<>>
        end,

        case BinProps of
            #{ <<"stub">> := true} ->
                Type = maps:get(<<"content_type">>, BinProps, ?DEFAULT_ATTACHMENT_CONTENT_TYPE),
                RevPos = maps:get(<<"revpos">>, BinProps, nil),
                DiskLen = maps:get(<<"length">>, BinProps, undefined),
                {Enc, EncLen} = att_encoding_info(BinProps),
                [#att{name=Name, data=stub, type=Type, att_len=EncLen,
                      disk_len=DiskLen, encoding=Enc, revpos=RevPos, md5=Md5} | Atts1];
            _ ->
                Type = maps:get(<<"content_type">>, BinProps, ?DEFAULT_ATTACHMENT_CONTENT_TYPE),
                RevPos = maps:get(<<"revpos">>, BinProps, 0),
                case BinProps of
                    #{ <<"follows">> := true } ->
                        DiskLen = maps:get(<<"length">>, BinProps, undefined),
                        {Enc, EncLen} = att_encoding_info(BinProps),
                        [#att{name=Name, data=follows, type=Type, encoding=Enc,
                              att_len=EncLen, disk_len=DiskLen, revpos=RevPos, md5=Md5} | Atts1];
                    _ ->
                        Bin = base64:decode(maps:get(<<"data">>, BinProps, <<"">>)),
                        LenBin = size(Bin),
                        [#att{name=Name, data=Bin, type=Type, att_len=LenBin,
                              disk_len=LenBin, revpos=RevPos} | Atts1]
                end
        end
    end, [], JsonBins),
    Doc#doc{atts=Atts};

transfer_fields1(<<"_revisions">>, Obj, Doc) ->
    RevIds = maps:get(<<"ids">>, Obj, undefined),
    Start = maps:get(<<"start">>, Obj, undefined),
    if
        not is_integer(Start) ->
            throw({doc_validation, "_revisions.start isn't an integer."});
        not is_list(RevIds) ->
            throw({doc_validation, "_revisions.ids isn't a array."});
        true ->
            ok
    end,
    [throw({doc_validation, "RevId isn't a string"}) ||  RevId <- RevIds, not is_binary(RevId)],
    RevIds2 = [parse_revid(RevId) || RevId <- RevIds],
    Doc#doc{revs={Start, RevIds2}};

transfer_fields1(<<"_deleted">>, B, Doc) when is_boolean(B) ->  Doc#doc{deleted=B};

% ignored fields
transfer_fields1(<<"_revs_info">>, _, Doc) ->  Doc;
transfer_fields1(<<"_local_seq">>, _, Doc) -> Doc;
transfer_fields1(<<"_conflicts">>, _, Doc) -> Doc;
transfer_fields1(<<"_deleted_conflicts">>, _, Doc) -> Doc;

% special fields for replication documents
transfer_fields1(<<"_replication_state">>, V, #doc{body=Body} = Doc) ->
    Doc#doc{body=Body#{<<"_replication_state">> => V}};
transfer_fields1(<<"_replication_state_time">>, V, #doc{body=Body} = Doc) ->
    Doc#doc{body=Body#{<<"_replication_state">> => V}};
transfer_fields1(<<"_replication_state_reason">>, V, #doc{body=Body} = Doc) ->
    Doc#doc{body=Body#{<<"_replication_state_reason">> => V}};
transfer_fields1(<<"_replication_id">>, V, #doc{body=Body} = Doc) ->
    Doc#doc{body=Body#{<<"_replication_idv">> => V}};
transfer_fields1(<<"_replication_stats">>, V, #doc{body=Body} = Doc) ->
    Doc#doc{body=Body#{<<"_replication_id">> => V}};

% unknown special field
transfer_fields1(<<"_",Name/binary>>, _, _) ->
    throw({doc_validation,
            list_to_binary(io_lib:format("Bad special document member: _~s", [Name]))});

transfer_fields1(K, V, #doc{body=Body}=Doc) ->
    Doc#doc{body=Body#{K => V}}.

att_encoding_info(BinProps) ->
    DiskLen = maps:get(<<"length">>, BinProps, undefined),
    case maps:get(<<"encoding">>, BinProps, undefined) of
    undefined ->
        {identity, DiskLen};
    Enc ->
        EncodedLen = maps:get(<<"encoded_length">>, BinProps, DiskLen),
        {list_to_existing_atom(binary_to_list(Enc)), EncodedLen}
    end.

to_doc_info(FullDocInfo) ->
    {DocInfo, _Path} = to_doc_info_path(FullDocInfo),
    DocInfo.

max_seq(Tree, UpdateSeq) ->
    FoldFun = fun({_Pos, _Key}, Value, _Type, MaxOldSeq) ->
        case Value of
            {_Deleted, _DiskPos, OldTreeSeq} ->
                % Older versions didn't track data sizes.
                erlang:max(MaxOldSeq, OldTreeSeq);
            {_Deleted, _DiskPos, OldTreeSeq, _Size} ->
                erlang:max(MaxOldSeq, OldTreeSeq);
            _ ->
                MaxOldSeq
        end
    end,
    couch_key_tree:fold(FoldFun, UpdateSeq, Tree).

to_doc_info_path(#full_doc_info{id=Id,rev_tree=Tree,update_seq=Seq}) ->
    RevInfosAndPath = [
        {#rev_info{
            deleted = element(1, LeafVal),
            body_sp = element(2, LeafVal),
            seq = element(3, LeafVal),
            rev = {Pos, RevId}
        }, Path} || {LeafVal, {Pos, [RevId | _]} = Path} <-
            couch_key_tree:get_all_leafs(Tree)
    ],
    SortedRevInfosAndPath = lists:sort(
            fun({#rev_info{deleted=DeletedA,rev=RevA}, _PathA},
                {#rev_info{deleted=DeletedB,rev=RevB}, _PathB}) ->
            % sort descending by {not deleted, rev}
            {not DeletedA, RevA} > {not DeletedB, RevB}
        end, RevInfosAndPath),
    [{_RevInfo, WinPath}|_] = SortedRevInfosAndPath,
    RevInfos = [RevInfo || {RevInfo, _Path} <- SortedRevInfosAndPath],
    {#doc_info{id=Id, high_seq=max_seq(Tree, Seq), revs=RevInfos}, WinPath}.




att_foldl(#att{data=Bin}, Fun, Acc) when is_binary(Bin) ->
    Fun(Bin, Acc);
att_foldl(#att{data={Fd,Sp},md5=Md5}, Fun, Acc) ->
    couch_stream:foldl(Fd, Sp, Md5, Fun, Acc);
att_foldl(#att{data=DataFun,att_len=Len}, Fun, Acc) when is_function(DataFun) ->
   fold_streamed_data(DataFun, Len, Fun, Acc).

range_att_foldl(#att{data={Fd,Sp}}, From, To, Fun, Acc) ->
   couch_stream:range_foldl(Fd, Sp, From, To, Fun, Acc).

att_foldl_decode(#att{data={Fd,Sp},md5=Md5,encoding=Enc}, Fun, Acc) ->
    couch_stream:foldl_decode(Fd, Sp, Md5, Enc, Fun, Acc);
att_foldl_decode(#att{data=Fun2,att_len=Len, encoding=identity}, Fun, Acc) ->
       fold_streamed_data(Fun2, Len, Fun, Acc).

att_to_bin(#att{data=Bin}) when is_binary(Bin) ->
    Bin;
att_to_bin(#att{data=Iolist}) when is_list(Iolist) ->
    iolist_to_binary(Iolist);
att_to_bin(#att{data={_Fd,_Sp}}=Att) ->
    iolist_to_binary(
        lists:reverse(att_foldl(
                Att,
                fun(Bin,Acc) -> [Bin|Acc] end,
                []
        ))
    );
att_to_bin(#att{data=DataFun, att_len=Len}) when is_function(DataFun)->
    iolist_to_binary(
        lists:reverse(fold_streamed_data(
            DataFun,
            Len,
            fun(Data, Acc) -> [Data | Acc] end,
            []
        ))
    ).

get_validate_doc_fun(#doc{body=Body}=DDoc) ->
    case maps:is_key(<<"validate_doc_update">>, Body) of
        false -> nil;
        true ->
            fun(EditDoc, DiskDoc, Ctx, SecObj) ->
                couch_query_servers:validate_doc_update(DDoc, EditDoc, DiskDoc, Ctx, SecObj)
            end
    end.


get_validate_read_doc_fun(#doc{body=Body}=DDoc) ->
    case maps:is_key(<<"validate_doc_read">>, Body) of
        false -> nil;
        true ->
            fun(Doc, Ctx, SecObj) ->
                couch_query_servers:validate_doc_read(DDoc, Doc, Ctx, SecObj)
            end
    end.


has_stubs(#doc{atts=Atts}) ->has_stubs(Atts);
has_stubs([]) -> false;
has_stubs([#att{data=stub}|_]) -> true;
has_stubs([_Att|Rest]) -> has_stubs(Rest).

merge_stubs(#doc{id = Id}, nil) ->
    throw({missing_stub, <<"Previous revision missing for document ", Id/binary>>});
merge_stubs(#doc{id=Id,atts=MemBins}=StubsDoc, #doc{atts=DiskBins}) ->
    BinDict = dict:from_list([{Name, Att} || #att{name=Name}=Att <- DiskBins]),
    MergedBins = lists:map(
        fun(#att{name=Name, data=stub, revpos=StubRevPos}) ->
            case dict:find(Name, BinDict) of
            {ok, #att{revpos=DiskRevPos}=DiskAtt}
                    when DiskRevPos == StubRevPos orelse StubRevPos == nil ->
                DiskAtt;
            _ ->
                throw({missing_stub,
                        <<"id:", Id/binary, ", name:", Name/binary>>})
            end;
        (Att) ->
            Att
        end, MemBins),
    StubsDoc#doc{atts= MergedBins}.

fold_streamed_data(_RcvFun, 0, _Fun, Acc) ->
    Acc;
fold_streamed_data(RcvFun, LenLeft, Fun, Acc) when LenLeft > 0->
    Bin = RcvFun(),
    ResultAcc = Fun(Bin, Acc),
    fold_streamed_data(RcvFun, LenLeft - size(Bin), Fun, ResultAcc).

len_doc_to_multi_part_stream(Boundary, JsonBytes, Atts, SendEncodedAtts) ->
    len_doc_to_multi_part_stream(Boundary, JsonBytes, Atts,
                                 SendEncodedAtts, false).

len_doc_to_multi_part_stream(Boundary, JsonBytes, Atts, SendEncodedAtts,
                             ForceMp) ->

    AttsSize = lists:foldl(fun(Att, AccAttsSize) ->
            #att{
                data=Data,
                name=Name,
                att_len=AttLen,
                disk_len=DiskLen,
                type=Type,
                encoding=Encoding
            } = Att,
            case Data of
            stub ->
                AccAttsSize;
            _ ->
                AccAttsSize +
                4 + % "\r\n\r\n"
                case SendEncodedAtts of
                true ->
                    % header
                    length(integer_to_list(AttLen)) +
                    AttLen;
                _ ->
                    % header
                    length(integer_to_list(DiskLen)) +
                    DiskLen
                end +
                4 + % "\r\n--"
                size(Boundary) +

                % attachment headers
                % (the length of the Content-Length has already been set)
                size(Name) +
                size(Type) +
                length("\r\nContent-Disposition: attachment; filename=\"\"") +
                length("\r\nContent-Type: ") +
                length("\r\nContent-Length: ") +
                case Encoding of
                identity ->
                    0;
                 _ ->
                    length(atom_to_list(Encoding)) +
                    length("\r\nContent-Encoding: ")
                end
            end
        end, 0, Atts),
    case AttsSize of
        0 when ForceMp /= true ->
            {<<"application/json">>, iolist_size(JsonBytes)};
        0 ->
            {<<"multipart/related; boundary=\"", Boundary/binary, "\"">>,
                2 + % "--"
                size(Boundary) +
                36 + % "\r\ncontent-type: application/json\r\n\r\n"
                iolist_size(JsonBytes) +
                4 + % "\r\n--"
                size(Boundary) +
                2};
        _ ->
            {<<"multipart/related; boundary=\"", Boundary/binary, "\"">>,
                2 + % "--"
                size(Boundary) +
                36 + % "\r\ncontent-type: application/json\r\n\r\n"
                iolist_size(JsonBytes) +
                4 + % "\r\n--"
                size(Boundary) +
                + AttsSize +
                2 % "--"
                }
    end.

doc_to_multi_part_stream(Boundary, JsonBytes, Atts, WriteFun,
                         SendEncodedAtts) ->
    doc_to_multi_part_stream(Boundary, JsonBytes, Atts, WriteFun,
                         SendEncodedAtts, false).

doc_to_multi_part_stream(Boundary, JsonBytes, Atts, WriteFun,
    SendEncodedAtts, ForceMp) ->
    case lists:any(fun(#att{data=Data})-> Data /= stub end, Atts) of
    true ->
        WriteFun([<<"--", Boundary/binary,
                "\r\nContent-Type: application/json\r\n\r\n">>,
                JsonBytes, <<"\r\n--", Boundary/binary>>]),
        atts_to_mp(Atts, Boundary, WriteFun, SendEncodedAtts);
    false when ForceMp ->
        WriteFun([<<"--", Boundary/binary,
                "\r\nContent-Type: application/json\r\n\r\n">>,
                  JsonBytes, <<"\r\n--", Boundary/binary, "--" >>]);
    false ->
        WriteFun(JsonBytes)
    end.

atts_to_mp([], _Boundary, WriteFun, _SendEncAtts) ->
    WriteFun(<<"--">>);
atts_to_mp([#att{data=stub} | RestAtts], Boundary, WriteFun,
        SendEncodedAtts) ->
    atts_to_mp(RestAtts, Boundary, WriteFun, SendEncodedAtts);
atts_to_mp([Att | RestAtts], Boundary, WriteFun,
        SendEncodedAtts)  ->
    #att{
        name=Name,
        att_len=AttLen,
        disk_len=DiskLen,
        type=Type,
        encoding=Encoding
    } = Att,

    % write headers
    LengthBin = case SendEncodedAtts of
    true -> list_to_binary(integer_to_list(AttLen));
    false -> list_to_binary(integer_to_list(DiskLen))
    end,
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
    AttFun = case SendEncodedAtts of
    false ->
        fun att_foldl_decode/3;
    true ->
        fun att_foldl/3
    end,
    AttFun(Att, fun(Data, _) -> WriteFun(Data) end, ok),
    WriteFun(<<"\r\n--", Boundary/binary>>),
    atts_to_mp(RestAtts, Boundary, WriteFun, SendEncodedAtts).


doc_from_multi_part_stream(ContentType, DataFun) ->
    Parent = self(),
    Parser = spawn_link(fun() ->
        {<<"--",_/binary>>, _, _} = couch_httpd:parse_multipart_request(
            ContentType, DataFun,
            fun(Next) -> mp_parse_doc(Next, []) end),
        unlink(Parent),
        Parent ! {self(), finished}
        end),
    Ref = make_ref(),
    Parser ! {get_doc_bytes, Ref, self()},
    receive
    {doc_bytes, Ref, DocBytes} ->
        Doc = from_json_obj(?JSON_DECODE(DocBytes)),
        % go through the attachments looking for 'follows' in the data,
        % replace with function that reads the data from MIME stream.
        ReadAttachmentDataFun = fun() ->
            Parser ! {get_bytes, Ref, self()},
            receive {bytes, Ref, Bytes} -> Bytes end
        end,
        Atts2 = lists:map(
            fun(#att{data=follows}=A) ->
                A#att{data=ReadAttachmentDataFun};
            (A) ->
                A
            end, Doc#doc.atts),
        WaitFun = fun() ->
            receive {Parser, finished} -> ok end,
            erlang:put(mochiweb_request_recv, true)
        end,
        {ok, Doc#doc{atts=Atts2}, WaitFun, Parser}
    end.

mp_parse_doc({headers, H}, []) ->
    case proplists:get_value("content-type", H) of
    {"application/json", _} ->
        fun (Next) ->
            mp_parse_doc(Next, [])
        end
    end;
mp_parse_doc({body, Bytes}, AccBytes) ->
    fun (Next) ->
        mp_parse_doc(Next, [Bytes | AccBytes])
    end;
mp_parse_doc(body_end, AccBytes) ->
    receive {get_doc_bytes, Ref, From} ->
        From ! {doc_bytes, Ref, lists:reverse(AccBytes)}
    end,
    fun mp_parse_atts/1.

mp_parse_atts(eof) ->
    ok;
mp_parse_atts({headers, _H}) ->
    fun mp_parse_atts/1;
mp_parse_atts({body, Bytes}) ->
    receive {get_bytes, Ref, From} ->
        From ! {bytes, Ref, Bytes}
    end,
    fun mp_parse_atts/1;
mp_parse_atts(body_end) ->
    fun mp_parse_atts/1.


abort_multi_part_stream(Parser) ->
    abort_multi_part_stream(Parser, erlang:monitor(process, Parser)).

abort_multi_part_stream(Parser, MonRef) ->
    case is_process_alive(Parser) of
    true ->
        Parser ! {get_bytes, nil, self()},
        receive
        {bytes, nil, _Bytes} ->
             abort_multi_part_stream(Parser, MonRef);
        {'DOWN', MonRef, _, _, _} ->
             ok
        end;
    false ->
        erlang:demonitor(MonRef, [flush])
    end.


with_ejson_body(#doc{body = Body} = Doc) when is_binary(Body) ->
    Doc#doc{body = couch_compress:decompress(Body)};
with_ejson_body(#doc{body = Body} = Doc) when is_map(Body)->
    Doc.


load(Db, #doc_info{}=DI, Opts) ->
    Deleted = lists:member(deleted, Opts),
    case (catch couch_db:open_doc(Db, DI, Opts)) of
        {ok, #doc{deleted=false}=Doc} -> Doc;
        {ok, #doc{deleted=true}=Doc} when Deleted -> Doc;
        _Else -> null
    end;
load(Db, {DocId, Rev}, Opts) ->
    case (catch load(Db, DocId, Rev, Opts)) of
        #doc{deleted=false} = Doc -> Doc;
        _ -> null
    end.


load(Db, DocId, Rev, Options) ->
    case Rev of
        nil -> % open most recent rev
            case (catch couch_db:open_doc(Db, DocId, Options)) of
                {ok, Doc} -> Doc;
                _Error -> null
            end;
        _ -> % open a specific rev (deletions come back as stubs)
            case (catch couch_db:open_doc_revs(Db, DocId, [Rev], Options)) of
                {ok, [{ok, Doc}]} -> Doc;
                {ok, [{{not_found, missing}, Rev}]} -> null;
                {ok, [_Else]} -> null
            end
    end.

