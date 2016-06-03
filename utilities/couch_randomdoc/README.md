# couch_randomdoc


`couch_randomdoc` is a simple couchdb module to add support of random
document fetching. It allows you to fetch doc via teh HTTP Api, filter
them and even render them dirrently using a show function.

This extension is included in
[rcouch](https://github.com/refuge/rcouch).

## Contribute

For issues, comments or feedback please [create an issue](https://jira.refuge.io/browse/RCOUCH).

## HTTP API

To get a random doc do a GET to `/<db>/_random_doc` .

All options od the couchdb DOC APII can be applied.

An optionna parameter `filter` allows you to filter random document you
want to return. a filter functions is just like changes filters:

    GET /<db>/_random_doc?filter=DesignName/FuncName

There are also builtin filters:

- `_prefix`: prefix you want to filter, Pass the prefix using the
  `prefix` parameter:

        GET /<db>/_random_doc?filter=_prefix&prefix=someprefix

- `_design_doc`: to only return design document

- `_view`: get a random document from a view index. Pass the view name
  uing the `view` parameter:

        GET /<db>/_random_doc?filter=_view&view=DesignName/ViewName


## SHOW function

Like show function you may want to render your random documents
diffently:

        GET /<db>/_design/DesignName/_random/ShowFunction


Functions are put in the shows property of the design document like any
shows functions.
