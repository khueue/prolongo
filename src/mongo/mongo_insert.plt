:- include(misc(common)).

:- use_module(mongo_test_helper, []).
:- use_module(bson(bson), []).
:- use_module(misc(util), []).

:- begin_tests('mongo:insert/2').

test('insert', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    util:ms_since_epoch(MilliSeconds),
    Doc =
    [
        hello - [åäö,5.05],
        now   - utc(MilliSeconds)
    ],
    mongo:delete(Coll, [hello-[åäö,5.05]]),
    mongo:insert(Coll, Doc).

:- end_tests('mongo:insert/2').
