/*
DESCRIPTION:
This script can be used to remove unused databases which are generated while running the tests.
All databases starting with 'scalatest' will be removed.

SYNOPSIS:
mongo < remove_scalatest_dbs.js

*/



var conn = new Mongo();

db = conn.getDB("test");

var dbs = db.adminCommand('listDatabases');

dbs.databases.forEach(function(oneDb){
 if(!oneDb.name.indexOf('scalatest')){
	print("remove ", oneDb.name);
	var selDb = db.getSiblingDB(oneDb.name);
        selDb.dropDatabase();
 }

});
