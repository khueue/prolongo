:- include(misc(common)).

:- use_module(misc(util), []).

database('prolongo_test').
collection('testcoll').
database_dot_collection('prolongo_test.testcoll').

:- begin_tests('mongo:insert/3').

test('insert',
[
    setup(mongo:new_mongo(Mongo)),
    cleanup(mongo:free_mongo(Mongo))
]) :-
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

test('drop collection',
[
    setup(mongo:new_mongo(Mongo)),
    cleanup(mongo:free_mongo(Mongo))
]) :-
    Command =
    [
        drop - Coll
    ],
    collection(Coll),
    database(Db),
    mongo:command(Mongo, Command, Db, Result),
    bson:doc_get(Result, ok, 1.0).

/*
% Takes a bit too long when MongoDB reallocates the collection later.
test('drop database',
[
    setup(mongo:new_mongo(Mongo)),
    cleanup(mongo:free_mongo(Mongo))
]) :-
    Command =
    [
        dropDatabase - 1
    ],
    database(Db),
    mongo:command(Mongo, Command, Db).
*/

:- end_tests('mongo:command/3').
