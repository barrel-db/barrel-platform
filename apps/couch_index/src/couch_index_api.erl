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

-module(couch_index_api).

-include_lib("apps/couch/include/couch_db.hrl").

-type db() :: #db{}.

-callback get(Field::atom(), State::any()) -> ok | {error, term()}.

-callback init(Db::db(), Ddoc::binary()) -> ok | {error, term()}.

-callback open(Db::db(), State::any()) -> ok | {error, term()}.

-callback close(State::any()) -> ok | {error, term()}.

-callback delete(State::any()) -> ok | {error, term()}.

-callback reset(State::any()) -> ok | {error, term()}.

-callback start_update(State::any(), PurgedState::any(), NumChanges::integer()) 
    -> {ok, State::any()} | {error, term()}.

-callback purge(Db::db(), PurgeSeq::integer(), PurgedIdRevs::[any()], State::any()) 
    -> ok | {error, term()}.

-callback process_doc(Doc::{[]}, Seq::integer(), State::any()) -> ok | {error, term()}.

-callback finish_update(State::any()) -> {ok, State::any()} | {error, term()}.

-callback commit(State::any()) -> ok | {error, term()}.

-callback compact(Parent::pid(), State::any(), Opts::list()) -> ok | {error, term()}.

-callback swap_compacted(OldState::any(), NewState::any()) -> ok | {error, term()}.