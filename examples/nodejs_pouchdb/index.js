var PouchDB = require('pouchdb');
PouchDB.debug.enable('*');

var localDb = new PouchDB('test');
var remoteDb = new PouchDB('http://127.0.0.1:5984/testdb');

remoteDb.replicate.to(localDb, {filter: "_view",
                                 view: "test/test",
                                 query_params: { key: JSON.stringify("test") }
}).on('complete', function(info) {
    console.log(info);

    console.log("all docs are:");
    localDb.allDocs({
          include_docs: true,
            attachments: true
    }).then(function (result) {
        console.log(result);
    }).catch(function (err) {
          console.log(err);
    });
}).on('denied', function(info) {
    console.log(info);
}).on('error', function(err) {
    console.log(err);
});

