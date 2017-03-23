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

-module(barrel_httpc_attachments).
-author("Bernard Notarianni").

-export([attach/4]).
-export([attach/5]).
-export([get_attachment/4]).
-export([get_attachment_binary/4]).
-export([replace_attachment/5]).
-export([replace_attachment_binary/5]).
-export([delete_attachment/4]).
-export([attachments/3]).


-define(ATTTAG, <<"_attachments">>).


-spec attach(Conn, DocId, AttDescription, Options) -> Res when
    Conn :: barrel_httpc:conn(),
    DocId :: barrel_httpc:docid(),
    AttDescription :: barrel_httpc:att_description(),
    Options :: barrel_httpc:read_options(),
    Res :: {ok, barrel_httpc:docid(), barrel_httpc:rev()} | ok | {error, term()}.
attach(Conn, DocId, AttDescription, Options) ->
  case barrel_httpc:get(Conn, DocId, [{attachments, all}|Options]) of
    {ok, Doc, Attachments, Meta} ->
      AttId = maps:get(<<"id">>, AttDescription),
      AttOpts = [{rev, maps:get(<<"rev">>, Meta)}],
      case find_att_doc(Attachments, AttId) of
        {ok, _} -> {error, attachment_conflict};
        {error, not_found} ->
          Attachments2 = [AttDescription|Attachments],
          barrel_httpc:put(Conn, Doc, Attachments2, AttOpts)
      end;
    {error, _} = Error ->
      Error
  end.

-spec attach(Conn, DocId, AttDescription, AttBin, Options) -> Res when
    Conn :: barrel_httpc:conn(),
    DocId :: barrel_httpc:docid(),
    AttDescription :: barrel_httpc:att_description(),
    AttBin :: binary(),
    Options :: barrel_httpc:read_options(),
    Res :: {ok, barrel_httpc:docid(), barrel_httpc:rev()} | ok | {error, term()}.
attach(Conn, DocId, AttDescription, AttBin, Options) when is_binary(AttBin) ->
  attach(Conn, DocId, AttDescription#{<<"blob">> => AttBin}, Options).

-spec get_attachment(Conn, DocId, AttId, Options) -> Res when
    Conn :: barrel_httpc:conn(),
    DocId :: barrel_httpc:docid(),
    AttId :: binary(),
    Options :: barrel_httpc:write_options(),
    AttDescription :: barrel_httpc:att_description(),
    Res :: {ok, AttDescription} | {error, term()}.
get_attachment(Conn, DocId, AttId, Options) ->
  case barrel_httpc:get(Conn, DocId, [{attachments, all}|Options]) of
    {ok, _, Attachments, _} ->
      find_att_doc(Attachments, AttId);
    {error, _} = Error ->
      Error
  end.

-spec get_attachment_binary(Conn, DocId, AttId, Options) -> Res when
    Conn :: barrel_httpc:conn(),
    DocId :: barrel_httpc:docid(),
    AttId :: binary(),
    AttBin :: binary(),
    Options :: barrel_httpc:read_options(),
    Res :: {ok, AttBin} | {error, not_found}.
get_attachment_binary(Conn, DocId, AttId, Options) ->
  case barrel_httpc:get(Conn, DocId, [{attachments, all}|Options]) of
    {ok, _, Attachments, _} ->
      case find_att_doc(Attachments, AttId) of
        {ok, Attachment} ->
          Data = maps:get(<<"blob">>, Attachment),
          {ok, Data};
        {error, not_found} ->
          {error, not_found}
      end;
    {error, _} = Error ->
      Error
  end.

-spec replace_attachment(Conn, DocId, AttId, AttDescription, Options) -> Res when
    Conn :: barrel_httpc:conn(),
    DocId :: barrel_httpc:docid(),
    AttId :: binary(),
    AttDescription :: barrel_httpc:att_description(),
    Options :: barrel_httpc:write_options(),
    Res :: {ok, barrel_httpc:docid(), barrel_httpc:rev()} | ok | {error, term()}.
replace_attachment(Conn, DocId, AttId, AttDescription, Options) ->
  {ok, Doc, Attachments, Meta} = barrel_httpc:get(Conn, DocId,
                                                  [{attachments, all}|Options]),
  AttId = maps:get(<<"id">>, AttDescription),
  AttOpts = [{rev, maps:get(<<"rev">>, Meta)}],
  %% make new attachment and update the doc
  NewAttachments = replace_att_doc(AttId, AttDescription, Attachments),
  barrel_httpc:put(Conn, Doc, NewAttachments, AttOpts).


-spec replace_attachment_binary(Conn, DocId, AttId, AttBin, Options) -> Res when
    Conn :: barrel_httpc:conn(),
    DocId :: barrel_httpc:docid(),
    AttId :: binary(),
    AttBin :: binary(),
    Options :: barrel_httpc:write_options(),
    Res :: {ok, barrel_httpc:docid(), barrel_httpc:rev()} | ok | {error, term()}.
replace_attachment_binary(Conn, DocId, AttId, AttBin, Options) when is_binary(AttBin) ->
  {ok, _Doc, Attachments, _Meta} = barrel_httpc:get(Conn, DocId, [{attachments, all}|Options]),
  case find_att_doc(Attachments, AttId) of
    {error, not_found} -> {error, not_found};
    {ok, Attachment} ->
      NewAttachment = Attachment#{<<"blob">> => AttBin},
      replace_attachment(Conn, DocId, AttId, NewAttachment, Options)
  end.

-spec delete_attachment(Conn, DocId, AttId, Options) -> Res when
    Conn :: barrel_httpc:conn(),
    DocId :: barrel_httpc:docid(),
    AttId :: binary(),
    Options :: barrel_httpc:write_options(),
    Res ::  {ok, barrel_httpc:docid(), barrel_httpc:rev()} | {error, term()}.
delete_attachment(Conn, DocId, AttId, Options) ->
  {ok, Doc, Attachments, Meta} = barrel_httpc:get(Conn, DocId,
                                                  [{attachments, all}|Options]),
  NewAttachments = delete_att_doc(AttId, Attachments),
  PutOpts = [{rev, maps:get(<<"rev">>, Meta)}],
  barrel_httpc:put(Conn, Doc, NewAttachments, PutOpts).

-spec attachments(Conn, DocId, Options) -> Attachments when
  Conn :: barrel_httpc:conn(),
  DocId :: barrel_httpc:docid(),
  Options :: barrel_httpc:read_options(),
  Attachments :: [barrel_httpc:attachment()].
attachments(Conn, DocId, Options) ->
  {ok, _, Attachments, _} = barrel_httpc:get(Conn, DocId, [{attachments, all}|Options]),
  Attachments.

%% =============================================================================
%% Internals
%% =============================================================================

find_att_doc([#{<<"id">> := AttId}=AttDoc|_], AttId) -> {ok, AttDoc};
find_att_doc([_|Rest], AttId) -> find_att_doc(Rest, AttId);
find_att_doc([], _AttId) -> {error, not_found}.

replace_att_doc(_,_, []) ->
  [];
replace_att_doc(AttId, NewAttDoc, [#{<<"id">> := AttId}|Tail]) ->
  [NewAttDoc|replace_att_doc(AttId, NewAttDoc, Tail)];
replace_att_doc(AttId, NewAttDoc, [Head|Tail]) ->
  [Head|replace_att_doc(AttId, NewAttDoc, Tail)].

delete_att_doc(_, []) ->
  [];
delete_att_doc(AttId, [#{<<"id">> := AttId}|Tail]) ->
  delete_att_doc(AttId, Tail);
delete_att_doc(AttId, [Head|Tail]) ->
  [Head|delete_att_doc(AttId, Tail)].
