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

-module(couch_users_db).

-export([before_doc_update/2, after_doc_read/2, strip_non_public_fields/1]).

-include("couch_db.hrl").

% If the request's userCtx identifies an admin
%   -> save_doc (see below)
%
% If the request's userCtx.name is null:
%   -> save_doc
%   // this is an anonymous user registering a new document
%   // in case a user doc with the same id already exists, the anonymous
%   // user will get a regular doc update conflict.
% If the request's userCtx.name doesn't match the doc's name
%   -> 404 // Not Found
% Else
%   -> save_doc
before_doc_update(Doc, #db{user_ctx = UserCtx} = Db) ->
    Name = barrel_lib:userctx_get(name, UserCtx),
    DocName = get_doc_name(Doc),
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        save_doc(Doc);
    _ when Name =:= DocName orelse Name =:= null ->
        save_doc(Doc);
    _ ->
        throw(not_found)
    end.

% If newDoc.password == null || newDoc.password == undefined:
%   ->
%   noop
% Else -> // calculate password hash server side
%    newDoc.password_sha = hash_pw(newDoc.password + salt)
%    newDoc.salt = salt
%    newDoc.password = null
save_doc(#doc{body=Body} = Doc) ->
    case maps:get(<<"password">>, Body, undefined) of
    null -> % server admins don't have a user-db password entry
        Doc;
    undefined ->
        Doc;
    ClearPassword ->
        Iterations = barrel_config:get_env(auth_pbkdf2_iterations),
        Salt = barrel_uuids:random(),
        DerivedKey = barrel_passwords:pbkdf2(ClearPassword, Salt, Iterations),

        Body1 = Body#{
            <<"password_scheme">> => <<"pbkdf2">>,
            <<"iterations">> => Iterations,
            <<"salt">> => Salt,
            <<"derived_key">> => DerivedKey
        },

        Body2 = maps:remove(<<"password">>, Body1),
        Doc#doc{body=Body2}
    end.

% If the doc is a design doc
%   If the request's userCtx identifies an admin
%     -> return doc
%   Else
%     -> 403 // Forbidden
% If the request's userCtx identifies an admin
%   -> return doc
% If the request's userCtx.name doesn't match the doc's name
%   -> 404 // Not Found
% Else
%   -> return doc
after_doc_read(#doc{id = << "_design", _/binary>>} = Doc, Db) ->
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        Doc;
    _ ->
        throw({forbidden,
        <<"Only administrators can view design docs in the users database.">>})
    end;
after_doc_read(Doc, #db{user_ctx = UserCtx} = Db) ->
    Name = barrel_lib:userctx_get(name, UserCtx),
    DocName = get_doc_name(Doc),
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        Doc;
    _ when Name =:= DocName ->
        Doc;
    _ ->
        Doc1 = strip_non_public_fields(Doc),
        case Doc1 of
            #doc{body=#{}} -> throw(not_found);
            _ -> Doc1
        end
    end.

get_doc_name(#doc{id= <<"org.barrel.user:", Name/binary>>}) -> Name;
get_doc_name(_) -> undefined.

strip_non_public_fields(#doc{body=Body}=Doc) ->
    Public = barrel_config:get_envt(public_fields),
    Body2 = maps:filter(fun(K, _V) -> lists:member(binary_to_list(K), Public) end, Body),
    Doc#doc{body=Body2}.
