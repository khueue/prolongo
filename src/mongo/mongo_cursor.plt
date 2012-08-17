:- include(misc(common)).

:- use_module(mongo_test_helper, []).
:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- begin_tests('mongo:cursor_kill/1').

test('cursor kill', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo_test_helper:create_n_docs(20, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find(Coll, [hello-world], [], 0, 10, Cursor, _NotAllDocs),
    mongo:cursor_kill(Cursor),
    mongo:cursor_get_more(Cursor, 10, [], _Cursor1).

:- end_tests('mongo:cursor_kill/1').

:- begin_tests('mongo:cursor_kill_batch/1').

test('cursor kill batch', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo_test_helper:create_n_docs(20, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find(Coll, [hello-world], [], 0, 10, Cursor1, _NotAllDocs1),
    mongo:find(Coll, [hello-world], [], 0, 10, Cursor2, _NotAllDocs2),
    mongo:cursor_kill_batch([Cursor1,Cursor2]),
    mongo:cursor_get_more(Cursor1, 10, [], _Cursor11),
    mongo:cursor_get_more(Cursor2, 10, [], _Cursor22).

:- end_tests('mongo:cursor_kill_batch/1').
