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

-module(barrel_attachments).
-author("Bernard Notarianni").

-export([attach/4]).
-export([attach/5]).
-export([get_attachment/4]).
-export([get_attachment_binary/4]).
-export([replace_attachment/5]).
-export([replace_attachment_binary/4]).
-export([delete_attachment/4]).
-export([attachments/3]).


-define(ATTTAG, <<"_attachments">>).

attach(Db, DocId, AttDescription, Options) ->
  {ok, Doc} = barrel_db:get(Db, DocId, Options),
  Attachments = maps:get(?ATTTAG, Doc, []),
  AttId = maps:get(<<"id">>, AttDescription),
  case find_att_doc(AttId, Attachments) of
    {ok, _} -> {error, attachment_conflict};
    {error, not_found} ->
      Attachments2 = [AttDescription|Attachments],
      barrel_db:put(Db, DocId, Doc#{?ATTTAG => Attachments2}, Options)
  end.

attach(_Db, _DocId, _AttDescription, _Binary, _Options) ->
  {error, not_implemented}.

get_attachment(Db, DocId, AttId, Options) ->
  {ok, Doc} = barrel_db:get(Db, DocId, Options),
  Attachments = maps:get(?ATTTAG, Doc, []),
  find_att_doc(AttId, Attachments).

get_attachment_binary(_Db, _DocId, _AttId, _Options) ->
  {error, not_implemented}.

replace_attachment(Db, DocId, AttId, AttDescription, Options) ->
  {ok, Doc} = barrel_db:get(Db, DocId, Options),
  Attachments = maps:get(?ATTTAG, Doc, []),
  AttId = maps:get(<<"id">>, AttDescription),
  NewAttachments = replace_att_doc(AttId, AttDescription, Attachments),
  barrel_db:put(Db, DocId, Doc#{?ATTTAG => NewAttachments}, Options).

replace_attachment_binary(_Db, _DocId, _AttId, _Binary) ->
  {error, not_implemented}.

delete_attachment(Db, DocId, AttId, Options) ->
  {ok, Doc} = barrel_db:get(Db, DocId, Options),
  Attachments = maps:get(?ATTTAG, Doc, []),
  NewAttachments = delete_att_doc(AttId, Attachments),
  barrel_db:put(Db, DocId, Doc#{?ATTTAG => NewAttachments}, Options).

attachments(Db, DocId, Options) ->
  {ok, Doc} = barrel_db:get(Db, DocId, Options),
  maps:get(?ATTTAG, Doc, []).


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
