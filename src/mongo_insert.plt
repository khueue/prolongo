:- include(include/common).

:- begin_tests('mongo:insert/2').

test('insert', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo_util:ms_since_epoch(MilliSeconds),
    Doc =
    [
        hello - [åäö,5.05],
        now   - utc(MilliSeconds)
    ],
    mongo:delete(Coll, [hello-[åäö,5.05]]),
    mongo:insert(Coll, Doc).

:- end_tests('mongo:insert/2').

:- begin_tests('mongo:insert_batch/3').

test('insert', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo_util:ms_since_epoch(MilliSeconds),
    Doc =
    [
        hello - world,
        now   - utc(MilliSeconds)
    ],
    mongo:delete(Coll, [hello-world]),
    mongo:insert_batch(Coll, [], [Doc,Doc,Doc]),
    mongo:find(Coll, [hello-world], [now-1], 0, 0, _Cursor, Docs),
    lists:length(Docs, 3).

:- end_tests('mongo:insert_batch/3').
