:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).

up(Conn, Coll) :-
    mongo:new_connection(Conn),
    mongo:get_database(Conn, 'prolongo', Db),
    mongo:get_collection(Db, 'testcoll', Coll).

down(Conn) :-
    mongo:free_connection(Conn).

create_n_docs(0, []) :- !.
create_n_docs(N, [[hello-world,number-N]|Docs]) :-
    N1 is N - 1,
    create_n_docs(N1, Docs).

:- begin_tests('mongo:upsert/3').

test('upsert', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:delete(Coll, [hello-me]),
    mongo:upsert(Coll, [hello-world], [hello-me]),
    mongo:find_one(Coll, [hello-me], Doc),
    bson:doc_get(Doc, hello, me).

:- end_tests('mongo:upsert/3').

:- begin_tests('mongo:update/3').

test('update normal', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:delete(Coll, [hello-me]),
    mongo:insert(Coll, [hello-world]),
    mongo:update(Coll, [hello-world], [hello-me]),
    mongo:find_one(Coll, [hello-me], Doc),
    bson:doc_get(Doc, hello, me).

:- end_tests('mongo:update/3').

:- begin_tests('mongo:update_all/3').

test('update multi', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:delete(Coll, [hello-me]),
    mongo:insert(Coll, [hello-world,num-1]),
    mongo:insert(Coll, [hello-world,num-2]),
    mongo:insert(Coll, [hello-world,num-3]),
    mongo:update_all(Coll, [hello-world], ['$inc'-[num-100]]),
    mongo:find(Coll, [hello-world], [], 0, 3, _Cursor, Docs),
    lists:length(Docs, 3),
    lists:member(Doc1, Docs),
    bson:doc_get(Doc1, num, 101),
    lists:member(Doc2, Docs),
    bson:doc_get(Doc2, num, 102),
    lists:member(Doc3, Docs),
    bson:doc_get(Doc3, num, 103),
    !. % Not interested in member choices.

:- end_tests('mongo:update_all/3').

:- begin_tests('mongo:delete/2').

test('delete', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:insert(Coll, [hello-world]),
    mongo:find(Coll, [hello-world], [], 0, 0, _Cursor1, [_|_]),
    mongo:delete(Coll, [hello-world]),
    mongo:find(Coll, [hello-world], [], 0, 0, _Cursor2, []).

:- end_tests('mongo:delete/2').

:- begin_tests('mongo:find/7').

test('cursor', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
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
    mongo:has_more(Cursor0),
    mongo:get_more(Cursor0, 3, Docs1, Cursor1),
    Docs1 =
    [
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_]
    ],
    mongo:has_more(Cursor1), % XXX Why still more, yet empty?
    mongo:get_more(Cursor1, 3, Docs2, Cursor2),
    Docs2 =
    [
    ],
    \+ mongo:has_more(Cursor2).

test('insert many single, cursor exhaust', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    insert_n_docs(Coll, 1000),
    mongo:find(Coll, [hello-world], [number-1], 0, 0, Cursor, Docs0),
    mongo:exhaust(Cursor, DocsRest),
    lists:length(Docs0, N0),
    lists:length(DocsRest, N),
    1000 is N0 + N.

insert_n_docs(_Coll, 0) :- !.
insert_n_docs(Coll, N) :-
    mongo:insert(Coll, [hello-world,number-N]),
    N1 is N - 1,
    insert_n_docs(Coll, N1).

test('insert batch, cursor exhaust', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    create_n_docs(1000, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find(Coll, [hello-world], [number-1], 0, 0, Cursor, Docs0),
    mongo:exhaust(Cursor, DocsRest),
    lists:length(Docs0, N0),
    lists:length(DocsRest, N),
    1000 is N0 + N.

test('insert batch, find_all', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    create_n_docs(1000, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find_all(Coll, [hello-world], [number-1], DocsAll),
    lists:length(DocsAll, 1000).

:- end_tests('mongo:find/7').

:- begin_tests('mongo:find/8').
:- end_tests('mongo:find/8').

:- begin_tests('mongo:kill/1'). % xxx better name?

test('cursor kill', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    create_n_docs(20, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find(Coll, [hello-world], [], 0, 10, Cursor, _NotAllDocs),
    mongo:kill(Cursor),
    mongo:get_more(Cursor, 10, [], _Cursor1).

:- end_tests('mongo:kill/1').

:- begin_tests('mongo:kill_batch/1'). % xxx name?

test('cursor kill batch', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:delete(Coll, [hello-world]),
    create_n_docs(20, Docs),
    mongo:insert_batch(Coll, [], Docs),
    mongo:find(Coll, [hello-world], [], 0, 10, Cursor1, _NotAllDocs1),
    mongo:find(Coll, [hello-world], [], 0, 10, Cursor2, _NotAllDocs2),
    mongo:kill_batch([Cursor1,Cursor2]),
    mongo:get_more(Cursor1, 10, [], _Cursor11),
    mongo:get_more(Cursor2, 10, [], _Cursor22).

:- end_tests('mongo:kill_batch/1').

:- begin_tests('mongo:find_one/3,4').

test('entire doc', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    Doc = [hello-world,number-42],
    mongo:delete(Coll, [hello-world]),
    mongo:insert(Coll, Doc),
    mongo:find_one(Coll, Doc, Doc1),
    bson:doc_get(Doc1, number, 42).

test('return fields selector', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    Doc = [hello-world,number-42],
    mongo:delete(Coll, [hello-world]),
    mongo:insert(Coll, Doc),
    Fields = [number-1],
    mongo:find_one(Coll, Doc, Fields, Doc1),
    bson:doc_get(Doc1, '_id', object_id(_)), % Always returned.
    bson:doc_get(Doc1, number, 42),
    \+ bson:doc_get(Doc1, hello, world).

:- end_tests('mongo:find_one/3,4').

:- begin_tests('mongo:insert/2').

test('insert', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    util:ms_since_epoch(MilliSeconds),
    Doc =
    [
        hello - [åäö,5.05],
        now   - utc(MilliSeconds)
    ],
    mongo:delete(Coll, [hello-[åäö,5.05]]),
    mongo:insert(Coll, Doc).

:- end_tests('mongo:insert/2').

:- begin_tests('mongo commands').

test('list commands', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:collection_database(Coll, Database),
    mongo:list_commands(Database, Doc),
    bson:doc_get_strict(Doc, commands, _).

test('list collection names', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:collection_database(Coll, Database),
    mongo:list_collection_names(Database, Names),
    lists:member('system.indexes', Names),
    lists:member('testcoll', Names),
    !. % Not interested in member choices.

test('list database infos', [setup(up(Conn,_Coll)),cleanup(down(Conn))]) :-
    mongo:list_database_infos(Conn, Infos),
    bson:doc_get_strict(Infos, 'prolongo', _).

test('list database names', [setup(up(Conn,_Coll)),cleanup(down(Conn))]) :-
    mongo:list_database_names(Conn, Names),
    lists:member('prolongo', Names),
    !. % Not interested in member choices.

test('drop collection', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:drop_collection(Coll).

test('drop database', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:collection_database(Coll, Database),
    mongo:drop_database(Database).

test('generic command', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:collection_database(Coll, Database),
    mongo:command(Database, [profile - -1], Doc),
    bson:doc_get_strict(Doc, ok, _).

test('command get last error', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo:collection_database(Coll, Database),
    mongo:get_last_error(Database, Doc),
    bson:doc_get_strict(Doc, ok, _).

:- end_tests('mongo commands').
