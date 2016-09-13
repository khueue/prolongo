:- include(include/common).

:- begin_tests('mongo:find/7').

test('query error on invalid selector doc', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn)),
        throws(mongo_error('server response error', [_ErrorDoc]))
    ]) :-
    mongo:find(Coll, [this_is-['$slice'-invalid]], [], 0, 0, _Cursor, _Docs).

test('cursor', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:insert(Coll, [hello-world,number-1]),
    mongo:insert(Coll, [hello-world,number-2]),
    mongo:insert(Coll, [hello-world,number-3]),
    mongo:insert(Coll, [hello-world,number-4]),
    mongo:insert(Coll, [hello-world,number-5]),
    mongo:insert(Coll, [hello-world,number-6]),
    mongo:insert(Coll, [hello-world,number-7]),
    mongo:insert(Coll, [hello-world,number-8]),
    mongo:insert(Coll, [hello-world,number-9]),
    mongo:find(Coll, [hello-world], [number-1], 3, 3, Cursor0, Docs0),
    Docs0 =
    [
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_]
    ],
    mongo:cursor_has_more(Cursor0),
    mongo:cursor_get_more(Cursor0, 3, Docs1, Cursor1),
    Docs1 =
    [
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_]
    ],
    mongo:cursor_has_more(Cursor1), % @tbd XXX Why still more, yet empty?
    mongo:cursor_get_more(Cursor1, 3, Docs2, Cursor2),
    Docs2 =
    [
    ],
    \+ mongo:cursor_has_more(Cursor2).

test('insert many single, cursor exhaust', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    insert_n_docs(Coll, 1000),
    mongo:find(Coll, [hello-world], [number-1], 0, 0, Cursor, Docs0),
    mongo:cursor_exhaust(Cursor, DocsRest),
    lists:length(Docs0, N0),
    lists:length(DocsRest, N),
    1000 is N0 + N.

insert_n_docs(_Coll, 0) :- !.
insert_n_docs(Coll, N) :-
    mongo:insert(Coll, [hello-world,number-N]),
    N1 is N - 1,
    insert_n_docs(Coll, N1).

test('insert batch, cursor exhaust', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo_test_helper:create_n_docs(1000, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find(Coll, [hello-world], [number-1], 0, 0, Cursor, Docs0),
    mongo:cursor_exhaust(Cursor, DocsRest),
    lists:length(Docs0, N0),
    lists:length(DocsRest, N),
    1000 is N0 + N.

test('insert batch, find all', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo_test_helper:create_n_docs(1000, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find_all(Coll, [hello-world], [number-1], DocsAll),
    lists:length(DocsAll, 1000).

:- end_tests('mongo:find/7').

:- begin_tests('mongo:find/8').
% XXX @tbd Add tests?
:- end_tests('mongo:find/8').

:- begin_tests('mongo:find_one/3,4').

test('entire doc', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    Doc = [hello-world,number-42],
    mongo:delete(Coll, [hello-world]),
    mongo:insert(Coll, Doc),
    mongo:find_one(Coll, Doc, Doc1),
    bson:doc_get(Doc1, number, 42).

test('doc not found returns nil', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    Doc = [hello-world,number-42],
    mongo:delete(Coll, [hello-world]),
    mongo:find_one(Coll, Doc, nil).

test('return fields selector', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    Doc = [hello-world,number-42],
    mongo:delete(Coll, [hello-world]),
    mongo:insert(Coll, Doc),
    Fields = [number-1],
    mongo:find_one(Coll, Doc, Fields, Doc1),
    bson:doc_get(Doc1, '_id', object_id(_)), % Always returned.
    bson:doc_get(Doc1, number, 42),
    \+ bson:doc_get(Doc1, hello, world).

:- end_tests('mongo:find_one/3,4').
