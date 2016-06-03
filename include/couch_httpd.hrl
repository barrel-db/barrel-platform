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

-record(doc_query_args, {
    options = [],
    rev = nil,
    open_revs = [],
    update_type = interactive_edit,
    atts_since = nil
}).


-record(all_docs_args, {preflight_fun,
        				start_key,
        				start_key_docid,
        				end_key,
        				end_key_docid,
        				keys,
        				direction = fwd,
        				limit = 16#10000000,
        				skip = 0,
        				group_level = 0,
        				stale = false,
        				inclusive_end = true,
        				include_docs = false,
        				doc_options = [],
        				update_seq=false,
        				conflicts,
        				callback,
        				list,
        				extra = []}).