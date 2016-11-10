%% Copyright 2016, Benoit Chesneau
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

-module(barrel).
-author("benoitc").

%% API
-export([all_databases/0]).

-export([
  start/3,
  stop/1,
  clean/1,
  infos/1,
  put/4,
  put_rev/5,
  get/3,
  delete/4,
  post/3,
  fold_by_id/4,
  changes_since/4,
  revsdiff/3
]).

-export([
  attach/4,
  attach/5,
  get_attachment/4,
  get_attachment_binary/4,
  replace_attachment/5,
  replace_attachment_binary/5,
  delete_attachment/4,
  attachments/3
]).


all_databases() ->
  {ok, Stores} = application:get_env(barrel, stores),
  AllDbs = lists:foldl(
    fun({Store, _, _}, Acc) ->
      Dbs = barrel_store:all_dbs(Store),
      Dbs ++ Acc
    end, [], Stores),
  lists:usort(AllDbs).



start(Name, Store, Options) ->
  barrel_db:start(Name, Store, Options).

stop(Name) ->
  barrel_db:stop(Name).

clean(Name) ->
  barrel_db:clean(Name).

infos(Name) ->
  barrel_db:infos(Name).

get(Db, DocId, Options) ->
  barrel_db:get(Db, DocId, Options).

put(Db, DocId, Body, Options) ->
  barrel_db:put(Db, DocId, Body, Options).

put_rev(Db, DocId, Body, History, Options) ->
  barrel_db:put_rev(Db, DocId, Body, History, Options).

delete(Db, DocId, RevId, Options) ->
  barrel_db:delete(Db, DocId, RevId, Options).

post(Db, Doc, Options) ->
  barrel_db:post(Db, Doc, Options).

fold_by_id(Db, Fun, Acc, Options) ->
  barrel_db:fold_by_id(Db, Fun, Acc, Options).

revsdiff(Db, DocId, RevIds) ->
  barrel_db:revsdiff(Db, DocId, RevIds).

changes_since(Db, Since, Fun, Acc) ->
  barrel_db:changes_since(Db, Since, Fun, Acc).



attach(Db, DocId, AttDescription, Options) ->
  barrel_attachments:attach(Db, DocId, AttDescription, Options).

attach(Db, DocId, AttDescription, Binary, Options) ->
  barrel_attachments:attach(Db, DocId, AttDescription, Binary, Options).

get_attachment(Db, DocId, AttId, Options) ->
  barrel_attachments:get_attachment(Db, DocId, AttId, Options).

get_attachment_binary(Db, DocId, AttId, Options) ->
  barrel_attachments:get_attachment_binary(Db, DocId, AttId, Options).

replace_attachment(Db, DocId, AttId, AttDescription, Options) ->
  barrel_attachments:replace_attachment(Db, DocId, AttId, AttDescription, Options).

replace_attachment_binary(Db, DocId, AttId, Binary, Options) ->
  barrel_attachments:replace_attachment_binary(Db, DocId, AttId, Binary, Options).

delete_attachment(Db, DocId, AttId, Options) ->
  barrel_attachments:delete_attachment(Db, DocId, AttId, Options).

attachments(Db, DocId, Options) ->
  barrel_attachments:attachments(Db, DocId, Options).
