:- include(misc(common)).

:- use_module(misc(util), []).

database('prolongo_test').
collection('testcoll').
database_dot_collection('prolongo_test.testcoll').

setup(Mongo) :-
    mongo:new_mongo(Mongo0),
    database(Database),
    mongo:use_database(Mongo0, Database, Mongo).

cleanup(Mongo) :-
    mongo:free_mongo(Mongo).

:- begin_tests('mongo:insert/3').

test('insert', [setup(setup(Mongo)),cleanup(cleanup(Mongo))]) :-
    util:ms_since_epoch(MilliSeconds),
    Document =
    [
        hello - [åäö,5.05],
        now   - utc(MilliSeconds)
    ],
    database_dot_collection(DbDotColl),
    mongo:insert(Mongo, Document, DbDotColl).

:- end_tests('mongo:insert/3').

:- begin_tests('mongo:command/3').

test('drop collection', [setup(setup(Mongo)),cleanup(cleanup(Mongo))]) :-
    collection(Coll),
    database(Db),
    mongo:drop_collection(Mongo, Db, Coll, Result),
    bson:doc_get(Result, ok, 1.0).

/*
% Takes a bit too long when MongoDB reallocates the collection later.
test('drop database', [setup(setup(Mongo)),cleanup(cleanup(Mongo))]) :-
    Command =
    [
        dropDatabase - 1
    ],
    database(Db),
    mongo:command(Mongo, Command, Db).
*/

:- end_tests('mongo:command/3').
