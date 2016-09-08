#!/usr/bin/env bash

can_get_database_informations() {
    curl -sfX GET http://localhost:8080/testdb > /dev/null
    assertion__status_code_is_success $?
}

can_put_get_delete_a_document() {
    JSON='{ "name": "tom", "lang": ["erlang", "python"] }'

    REPLY=$(curl -sfX PUT http://localhost:8080/testdb/cat -d "$JSON")
    assertion__status_code_is_success $?

    curl -sfX GET http://localhost:8080/testdb/cat > /dev/null
    assertion__status_code_is_success $?

    REVID="$(echo $REPLY | jq --raw-output .rev)"
    curl -sfX DELETE "http://localhost:8080/testdb/cat?rev=$REVID" > /dev/null
    assertion__status_code_is_success $?
}

can_retrieve_revdiffs() {
    JSON='{ "name": "tom", "lang": ["erlang", "python"] }'

    REPLY=$(curl -sfX PUT http://localhost:8080/testdb/cat -d "$JSON")
    assertion__status_code_is_success $?

    REVSDIFF=$(curl -sfX POST http://localhost:8080/testdb/_rev_diffs -d "{\"cat\": [\"1-md5\"]}")
    assertion__status_code_is_success $?
}

can_retrieve_changes() {
    JSON='{ "name": "tom", "lang": ["erlang", "python"] }'

    REPLY=$(curl -sfX PUT http://localhost:8080/testdb/dog -d "$JSON")
    assertion__status_code_is_success $?

    REVSDIFF=$(curl -sfX GET http://localhost:8080/testdb/_changes?since=0)
    assertion__status_code_is_success $?
}
