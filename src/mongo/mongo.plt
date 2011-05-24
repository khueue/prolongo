:- include(misc(common)).

:- use_module(misc(util), []).

up(Conn, Coll) :-
    mongo_connection:new(Conn),
    mongo_connection:get_database(Conn, 'prolongo', Db),
    mongo_database:get_collection(Db, 'testcoll', Coll).

down(Conn) :-
    mongo_connection:free(Conn).

:- begin_tests('mongo:update/4').

test('update normal', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo_delete:delete(Coll, [hello-world]),
    mongo_delete:delete(Coll, [hello-me]),
    mongo_insert:insert(Coll, [hello-world]),
    mongo_update:update(Coll, [hello-world], [hello-me]),
    mongo_find:find_one(Coll, [hello-me], Doc),
    bson:doc_get(Doc, hello, me).

:- end_tests('mongo:update/4').

:- begin_tests('mongo:update/5').

test('update upsert', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo_delete:delete(Coll, [hello-world]),
    mongo_delete:delete(Coll, [hello-me]),
    mongo_update:update(Coll, [hello-world], [hello-me], [upsert]),
    mongo_find:find_one(Coll, [hello-me], Doc),
    bson:doc_get(Doc, hello, me).

test('update multi', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo_delete:delete(Coll, [hello-world]),
    mongo_delete:delete(Coll, [hello-me]),
    mongo_insert:insert(Coll, [hello-world,num-1]),
    mongo_insert:insert(Coll, [hello-world,num-2]),
    mongo_insert:insert(Coll, [hello-world,num-3]),
    mongo_update:update(Coll, [hello-world], ['$inc'-[num-10]], [multi]),
    mongo_find:find(Coll, [hello-world], [], 0, 3, _Cursor, [Doc1,Doc2,Doc3]),
    % XXX Maybe this sort order is not guaranteed?
    bson:doc_get(Doc1, num, 11),
    bson:doc_get(Doc2, num, 12),
    bson:doc_get(Doc3, num, 13).

:- end_tests('mongo:update/5').

:- begin_tests('mongo:delete/3').

test('delete', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo_insert:insert(Coll, [hello-world]),
    mongo_find:find(Coll, [hello-world], [], 0, 0, _Cursor1, [_|_]),
    mongo_delete:delete(Coll, [hello-world]),
    mongo_find:find(Coll, [hello-world], [], 0, 0, _Cursor2, []).

:- end_tests('mongo:delete/3').

:- begin_tests('mongo:find/8').

test('cursor', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo_delete:delete(Coll, [hello-world]),
    mongo_insert:insert(Coll, [hello-world,number-1]),
    mongo_insert:insert(Coll, [hello-world,number-2]),
    mongo_insert:insert(Coll, [hello-world,number-3]),
    mongo_insert:insert(Coll, [hello-world,number-4]),
    mongo_insert:insert(Coll, [hello-world,number-5]),
    mongo_insert:insert(Coll, [hello-world,number-6]),
    mongo_insert:insert(Coll, [hello-world,number-7]),
    mongo_insert:insert(Coll, [hello-world,number-8]),
    mongo_insert:insert(Coll, [hello-world,number-9]),
    mongo_find:find(Coll, [hello-world], [number-1], 3, 3, Cursor0, Docs0),
    Docs0 =
    [
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_]
    ],
    mongo_cursor:has_more(Cursor0),
    mongo_cursor:get_more(Cursor0, 3, Docs1, Cursor1),
    Docs1 =
    [
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_],
        ['_id'-object_id(_),number-_]
    ],
    mongo_cursor:has_more(Cursor1),
    mongo_cursor:get_more(Cursor1, 3, Docs2, Cursor2),
    Docs2 =
    [
    ],
    \+ mongo_cursor:has_more(Cursor2).

test('insert many single, cursor exhaust', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo_delete:delete(Coll, [hello-world]),
    insert_n_docs(Coll, 1000),
    mongo_find:find(Coll, [hello-world], [number-1], 0, 0, Cursor, Docs0),
    mongo_cursor:exhaust(Cursor, DocsRest),
    lists:length(Docs0, N0),
    lists:length(DocsRest, N),
    1000 is N0 + N.

insert_n_docs(_Coll, 0) :- !.
insert_n_docs(Coll, N) :-
    mongo_insert:insert(Coll, [hello-world,number-N]),
    N1 is N - 1,
    insert_n_docs(Coll, N1).

test('insert batch, cursor exhaust', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    mongo_delete:delete(Coll, [hello-world]),
    create_n_docs(1000, Docs),
    mongo_insert:insert_batch(Coll, [], Docs),
    mongo_find:find(Coll, [hello-world], [number-1], 0, 0, Cursor, Docs0),
    mongo_cursor:exhaust(Cursor, DocsRest),
    lists:length(Docs0, N0),
    lists:length(DocsRest, N),
    1000 is N0 + N.

create_n_docs(0, []) :- !.
create_n_docs(N, [[hello-world,number-N]|Docs]) :-
    N1 is N - 1,
    create_n_docs(N1, Docs).

:- end_tests('mongo:find/8').

:- begin_tests('mongo:find_one/4,5').

test('entire doc', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    Doc = [hello-world, number-42],
    mongo_insert:insert(Coll, Doc),
    mongo_find:find_one(Coll, Doc, Doc1),
    bson:doc_get(Doc1, number, 42).

test('entire doc', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    Doc = [hello-world, number-42],
    mongo_insert:insert(Coll, Doc),
    mongo_find:find_one(Coll, Doc, Doc1),
    bson:doc_get(Doc1, number, 42).

test('return fields selector', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    Doc = [hello-world, number-42],
    mongo_insert:insert(Coll, Doc),
    Fields = [number-1],
    mongo_find:find_one(Coll, Doc, Fields, Doc1),
    bson:doc_get(Doc1, '_id', object_id(_)), % Always returned.
    bson:doc_get(Doc1, number, 42),
    \+ bson:doc_get(Doc1, hello, world).

:- end_tests('mongo:find_one/4,5').

:- begin_tests('mongo:insert/3').

test('insert', [setup(up(Conn,Coll)),cleanup(down(Conn))]) :-
    util:ms_since_epoch(MilliSeconds),
    Doc =
    [
        hello - [åäö,5.05],
        now   - utc(MilliSeconds)
    ],
    mongo_insert:insert(Coll, Doc).

:- end_tests('mongo:insert/3').

/*
:- begin_tests('mongo:command/3').

test('list database infos', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    mongo:list_database_infos(Mongo, DatabaseInfos),
    database(Database),
    bson:doc_get(DatabaseInfos, Database, Info),
    Info \== +null.
    %bson_format:pp(DatabaseInfos), nl.

test('list database names', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    mongo:list_database_names(Mongo, DatabaseNames),
    database(Database),
    core:memberchk(Database, DatabaseNames).

% XXX this works now, but I need to fix cursors (only one is shown).
test('list collection names', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    mongo:list_collection_names(Mongo, Result),
    bson_format:pp(Result).

test('drop collection',
[
    setup(up(Mongo)),
    cleanup(down(Mongo))
]) :-
    collection(Collection),
    mongo:drop_collection(Mongo, Collection, Result),
    write(Result), nl,
    mongo:doc_ok(Result).

% Takes a bit too long when MongoDB reallocates the collection later.
test('drop database', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    database(Database),
    mongo:drop_database(Mongo, Database, Result),
    bson:doc_get(Result, ok, 1.0).

:- end_tests('mongo:command/3').
*/
