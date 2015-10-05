-record(changes_args, {
    feed = "normal",
    dir = fwd,
    since = 0,
    limit = 1000000000000000,
    style = main_only,
    heartbeat,
    timeout,
    filter = "",
    filter_fun,
    filter_args = [],
    include_docs = false,
    doc_options = [],
    conflicts = false,
    db_open_options = [],
    view_name
}).
