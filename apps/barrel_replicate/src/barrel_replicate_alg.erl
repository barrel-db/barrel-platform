%% Copyright 2017, Bernard Notarianni
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

-module(barrel_replicate_alg).
-author("Bernard Notarianni").

%% gen_server API
-export([
  replicate/4
]).


replicate(Source, Target, Changes, Metrics) ->
  {ok, Metrics2} = lists:foldl(fun(C, {ok, Acc}) ->
                           sync_change(Source, Target, C, Acc)
                       end, {ok, Metrics}, Changes),
  {ok, Metrics2}.

sync_change(Source, Target, Change, Metrics) ->
  #{<<"id">> := DocId, <<"changes">> := History} = Change,
  {ok, MissingRevisions, _PossibleAncestors} = revsdiff(Target, DocId, History),
  Metrics2 = lists:foldr(fun(Revision, Acc) ->
                             sync_revision(Source, Target, DocId, Revision, Acc)
                         end, Metrics, MissingRevisions),
  {ok, Metrics2}.

sync_revision(Source, Target, DocId, Revision, Metrics) ->
  {Doc, Metrics2} = read_doc_with_history(Source, DocId, Revision, Metrics),
  History = barrel_doc:parse_revisions(Doc),
  DocWithoutRevisions = maps:remove(<<"_revisions">>, Doc),
  Metrics3 = write_doc(Target, DocWithoutRevisions, History, Metrics2),
  Metrics3.

read_doc_with_history(Source, Id, Rev, Metrics) ->
  Get = fun() -> get(Source, Id, [{rev, Rev}, {history, true}]) end,
  case timer:tc(Get) of
    {Time, {ok, Doc}} ->
      Metrics2 = barrel_metrics:inc(docs_read, Metrics, 1),
      Metrics3 = barrel_metrics:update_times(doc_read_times, Time, Metrics2),
      {Doc, Metrics3};
    _ ->
      Metrics2 = barrel_metrics:inc(doc_read_failures, Metrics, 1),
      {undefined, Metrics2}
  end.

write_doc(_, undefined, _, Metrics) ->
  Metrics;
write_doc(Target, Doc, History, Metrics) ->
  PutRev = fun() -> put_rev(Target, Doc, History, []) end,
  case timer:tc(PutRev) of
    {Time, {ok, _, _}} ->
      Metrics2 = barrel_metrics:inc(docs_written, Metrics, 1),
      Metrics3 = barrel_metrics:update_times(doc_write_times, Time, Metrics2),
      Metrics3;
    {_, Error} ->
      lager:error(
        "replicate write error on dbid=~p for docid=~p: ~w",
        [Target, maps:get(<<"id">>, Doc, undefined), Error]
      ),
      barrel_metrics:inc(doc_write_failures, Metrics, 1)
  end.

get({Mod, ModState}, Id, Opts) ->
  Mod:get(ModState, Id, Opts);
get(Db, Id, Opts) when is_binary(Db) ->
  barrel_db:get(Db, Id, Opts).

put_rev({Mod, ModState}, Doc, History, Opts) ->
  Mod:put_rev(ModState, Doc, History, Opts);
put_rev(Db, Doc, History, Opts) when is_binary(Db) ->
  barrel_local:put_rev(Db, Doc, History, Opts).

revsdiff({Mod, ModState}, DocId, History) ->
  Mod:revsdiff(ModState, DocId, History);
revsdiff(Conn, DocId, History) ->
  barrel_local:revsdiff(Conn, DocId, History).
