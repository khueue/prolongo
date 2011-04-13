:- include(misc(common)).

test_database('prolongo_test_1234321').
test_collection('testcoll').
test_database_dot_collection('prolongo_test_1234321.testcoll').

ms_since_epoch(MilliSeconds) :-
    core:get_time(FloatSeconds),
    FloatMilliSeconds is FloatSeconds * 1000,
    MilliSeconds is floor(FloatMilliSeconds).

:- begin_tests('mongo:command/3').

test('drop collection') :-
    test_collection(Coll),
    Command =
    [
        drop = Coll
    ],
    mongo:new_mongo(Mongo),
    test_database(Db),
    mongo:command(Mongo, Command, Db),
    mongo:free_mongo(Mongo).

:- end_tests('mongo:command/3').

:- begin_tests('mongo:insert/3').

test('insert') :-
    ms_since_epoch(MilliSeconds),
    Document =
    [
        hello = [åäö,5.05],
        now   = utc(MilliSeconds)
    ],
    mongo:new_mongo(Mongo),
    test_database_dot_collection(DbDotColl),
    mongo:insert(Mongo, Document, DbDotColl),
    mongo:free_mongo(Mongo).

:- end_tests('mongo:insert/3').
