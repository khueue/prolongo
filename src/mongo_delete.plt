:- include(include/common).

:- begin_tests('mongo:delete/2').

test('delete', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:insert(Coll, [hello-world]),
    mongo:insert(Coll, [hello-world]),
    mongo:find(Coll, [hello-world], [], 0, 0, _Cursor1, [_,_]),
    mongo:delete(Coll, [hello-world]),
    mongo:find(Coll, [hello-world], [], 0, 0, _Cursor2, []).

:- end_tests('mongo:delete/2').

:- begin_tests('mongo:delete/3').

test('delete single', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:insert(Coll, [hello-world]),
    mongo:insert(Coll, [hello-world]),
    mongo:delete(Coll, [hello-world], [single_remove]),
    mongo:find(Coll, [hello-world], [], 0, 0, _Cursor1, [_Doc]).

:- end_tests('mongo:delete/3').

