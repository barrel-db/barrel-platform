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

attach(Conn, DocId, AttDescription, Options) ->
  {ok, Doc, Attachments, Meta} = barrel_httpc:get(Conn, DocId, [{attachments, all}|Options]),
  AttId = maps:get(<<"id">>, AttDescription),
  AttOpts = [{rev, maps:get(<<"rev">>, Meta)}],
  case find_att_doc(AttId, Attachments) of
    {ok, _} -> {error, attachment_conflict};
    {error, not_found} ->
      Attachments2 = [AttDescription|Attachments],
      barrel_httpc:put(Conn, Doc, Attachments2, AttOpts)
  end.

attach(Conn, DocId, AttDescription, Binary, Options) ->
  attach(Conn, DocId, AttDescription#{<<"blob">> => Binary}, Options).

get_attachment(Conn, DocId, AttId, Options) ->
  {ok, _, Attachments, _} = barrel_httpc:get(Conn, DocId,
                                             [{attachments, all}|Options]),
  find_att_doc(AttId, Attachments).

get_attachment_binary(Conn, DocId, AttId, Options) ->
  {ok, _, Attachments, _} = barrel_httpc:get(Conn, DocId, 
                                             [{attachments, all}|Options]),
  case find_att_doc(AttId, Attachments) of
    {error, not_found} -> {error, not_found};
    {ok, Attachment} ->
      Data = maps:get(<<"blob">>, Attachment),
      {ok, Data}
  end.

replace_attachment(Conn, DocId, AttId, AttDescription, Options) ->
  {ok, Doc, Attachments, Meta} = barrel_httpc:get(Conn, DocId, 
                                                  [{attachments, all}|Options]),
  AttId = maps:get(<<"id">>, AttDescription),
  AttOpts = [{rev, maps:get(<<"rev">>, Meta)}],
  %% make new attachment and update the doc
  NewAttachments = replace_att_doc(AttId, AttDescription, Attachments),
  barrel_httpc:put(Conn, Doc, NewAttachments, AttOpts).

replace_attachment_binary(Conn, DocId, AttId, Binary, Options) ->
  {ok, _Doc, Attachments, _Meta} = barrel_httpc:get(Conn, DocId,
                                                    [{attachments, all}|Options]),
  case find_att_doc(AttId, Attachments) of
    {error, not_found} -> {error, not_found};
    {ok, Attachment} ->
      NewAttachment = Attachment#{<<"blob">> => Binary},
      replace_attachment(Conn, DocId, AttId, NewAttachment, Options)
  end.

delete_attachment(Conn, DocId, AttId, Options) ->
  {ok, Doc, Attachments, Meta} = barrel_httpc:get(Conn, DocId,
                                                  [{attachments, all}|Options]),
  NewAttachments = delete_att_doc(AttId, Attachments),
  PutOpts = [{rev, maps:get(<<"rev">>, Meta)}],
  barrel_httpc:put(Conn, Doc, NewAttachments, PutOpts).

attachments(Conn, DocId, Options) ->
  {ok, _, Attachments, _} = barrel_httpc:get(Conn, DocId,
                                             [{attachments, all}|Options]),
  Attachments.

%% =============================================================================
%% Internals
%% =============================================================================

find_att_doc(_, []) ->
  {error, not_found};
find_att_doc(AttId, [#{<<"id">> := AttId}=AttDoc|_]) ->
  {ok, AttDoc};
find_att_doc(AttId, [_|Tail]) ->
  find_att_doc(AttId, Tail).

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
