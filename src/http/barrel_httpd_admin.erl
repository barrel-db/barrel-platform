%% Copyright 2015-2016, Benoit Chesneau
%%
-module(barrel_httpd_admin).

-export([handle_req/1]).

handle_req(Req) ->
    case couch_httpd_request:method(Req) of
        'GET' -> do_handle_req(Req);
        _ -> couch_httpd:send_method_not_allowed(Req, "GET,HEAD")
    end.

do_handle_req(Req) ->
    Root = filename:join(code:priv_dir(barrel), "www"),
    case filelib:is_dir(Root) of
        true ->
            "/" ++ UrlPath = couch_httpd_request:path(Req),
            case couch_httpd:partition(UrlPath) of
                {_ActionKey, "/", RelativePath} ->
                    % GET /_utils/path or GET /_utils/
                    CachingHeaders = [{"Cache-Control", "private, must-revalidate"}],
                    couch_httpd:serve_file(Req, RelativePath, Root, CachingHeaders);
                {_ActionKey, "", _RelativePath} ->
                    % GET /_utils
                    RedirectPath = couch_httpd_request:path(Req) ++ "/", couch_httpd:send_redirect(Req, RedirectPath),
                    couch_httpd:send_redirect(Req, RedirectPath)
            end;
        false ->
            couch_httpd:send_error(Req, not_found)
    end.
