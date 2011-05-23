:- include(misc(common)).

:- use_module(misc(util), []).

database('prolongo').
collection('testcoll').

up(Mongo) :-
    mongo:new_mongo(Mongo0),
    database(Database),
    mongo:use_database(Mongo0, Database, Mongo).

down(Mongo) :-
    mongo:free_mongo(Mongo).

:- begin_tests('mongo:update/4').

test('update normal', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    mongo:delete(Mongo, Collection, [hello-world]),
    mongo:delete(Mongo, Collection, [hello-me]),
    mongo:insert(Mongo, Collection, [hello-world]),
    mongo:update(Mongo, Collection, [hello-world], [hello-me]),
    mongo:find_one(Mongo, Collection, [hello-me], Doc),
    bson:doc_get(Doc, hello, me).

:- end_tests('mongo:update/4').

:- begin_tests('mongo:update/5').

test('update upsert', [setup(up(Conn)),cleanup(down(Conn))]) :-
    % xxx collection(Collection),
    mongo:get_database(Conn, prolongo, Db),
    mongo:get_collection(Db, testcoll, Coll),
    mongo:delete(Coll, [hello-world]),
    mongo:delete(Coll, [hello-me]),
    mongo:update(Coll, [hello-world], [hello-me], [upsert]).
    %mongo:find_one(Coll, [hello-me], Doc),
    %bson:doc_get(Doc, hello, me).

test('update multi', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Coll),
    mongo:delete(Mongo, Coll, [hello-world]),
    mongo:delete(Mongo, Coll, [hello-me]),
    mongo:insert(Mongo, Coll, [hello-world,num-1]),
    mongo:insert(Mongo, Coll, [hello-world,num-2]),
    mongo:insert(Mongo, Coll, [hello-world,num-3]),
    mongo:update(Mongo, Coll, [hello-world], ['$inc'-[num-10]], [multi]),
    mongo:find(Mongo, Coll, [hello-world], [], 0, 3, _Cursor, [Doc1,Doc2,Doc3]),
    % XXX Maybe this sort order is not guaranteed?
    bson:doc_get(Doc1, num, 11),
    bson:doc_get(Doc2, num, 12),
    bson:doc_get(Doc3, num, 13).

:- end_tests('mongo:update/5').

:- begin_tests('mongo:delete/3').

test('delete', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    mongo:insert(Mongo, Collection, [hello-world]),
    mongo:find(Mongo, Collection, [hello-world], [], 0, 0, _Cursor1, [_|_]),
    mongo:delete(Mongo, Collection, [hello-world]),
    mongo:find(Mongo, Collection, [hello-world], [], 0, 0, _Cursor2, []).

:- end_tests('mongo:delete/3').

:- begin_tests('mongo:find/8').

test('cursor', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    mongo:delete(Mongo, Collection, [hello-world]),
    mongo:insert(Mongo, Collection, [hello-world,number-1]),
    mongo:insert(Mongo, Collection, [hello-world,number-2]),
    mongo:insert(Mongo, Collection, [hello-world,number-3]),
    mongo:insert(Mongo, Collection, [hello-world,number-4]),
    mongo:insert(Mongo, Collection, [hello-world,number-5]),
    mongo:insert(Mongo, Collection, [hello-world,number-6]),
    mongo:insert(Mongo, Collection, [hello-world,number-7]),
    mongo:insert(Mongo, Collection, [hello-world,number-8]),
    mongo:insert(Mongo, Collection, [hello-world,number-9]),
    mongo:find(Mongo, testcoll, [hello-world], [number-1], 3, 3, Cursor0, Docs0),
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
    mongo:cursor_has_more(Cursor1),
    mongo:cursor_get_more(Cursor1, 3, Docs2, Cursor2),
    Docs2 =
    [
    ],
    \+ mongo:cursor_has_more(Cursor2).

test('cursor exhaust', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    mongo:delete(Mongo, Collection, [hello-world]),
    mongo:insert(Mongo, Collection, [hello-world,number-1]),
    mongo:insert(Mongo, Collection, [hello-world,number-2]),
    mongo:insert(Mongo, Collection, [hello-world,number-3]),
    mongo:insert(Mongo, Collection, [hello-world,number-4]),
    mongo:insert(Mongo, Collection, [hello-world,number-5]),
    mongo:insert(Mongo, Collection, [hello-world,number-6]),
    mongo:insert(Mongo, Collection, [hello-world,number-7]),
    mongo:insert(Mongo, Collection, [hello-world,number-8]),
    mongo:insert(Mongo, Collection, [hello-world,number-9]),
    mongo:find(Mongo, testcoll, [hello-world], [number-1], 3, 3, Cursor0, Docs0),
    Docs0 =
    [
        ['_id'-object_id(_),number-4],
        ['_id'-object_id(_),number-5],
        ['_id'-object_id(_),number-6]
    ],
    mongo:cursor_exhaust(Cursor0, DocsRest),
    lists:length(DocsRest, 3).

:- end_tests('mongo:find/8').

:- begin_tests('mongo:find_one/4,5').

test('entire doc', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    Doc = [hello-world, number-42],
    mongo:insert(Mongo, Collection, Doc),
    mongo:find_one(Mongo, Collection, Doc, Doc1),
    bson:doc_get(Doc1, number, 42).

test('entire doc', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    Doc = [hello-world, number-42],
    mongo:insert(Mongo, Collection, Doc),
    mongo:find_one(Mongo, Collection, Doc, Doc1),
    bson:doc_get(Doc1, number, 42).

test('return fields selector', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    Doc = [hello-world, number-42],
    mongo:insert(Mongo, Collection, Doc),
    Fields = [number-1],
    mongo:find_one(Mongo, Collection, Doc, Fields, Doc1),
    bson:doc_get(Doc1, '_id', object_id(_)), % Always returned.
    bson:doc_get(Doc1, number, 42),
    \+ bson:doc_get(Doc1, hello, world).

:- end_tests('mongo:find_one/4,5').

:- begin_tests('mongo:insert/3').

test('insert', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    util:ms_since_epoch(MilliSeconds),
    Document =
    [
        hello - [åäö,5.05],
        now   - utc(MilliSeconds)
    ],
    collection(Collection),
    mongo:insert(Mongo, Collection, Document).

:- end_tests('mongo:insert/3').

:- begin_tests('mongo:command/3').
/*
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
*/

% XXX this works now, but I need to fix cursors (only one is shown).
test('list collection names', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    mongo:list_collection_names(Mongo, Result),
    bson_format:pp(Result).

/*
test('drop collection',
[
    setup(up(Mongo)),
    cleanup(down(Mongo))
]) :-
    collection(Collection),
    mongo:drop_collection(Mongo, Collection, Result),
    write(Result), nl,
    mongo:doc_ok(Result).
*/

/*
% Takes a bit too long when MongoDB reallocates the collection later.
test('drop database', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    database(Database),
    mongo:drop_database(Mongo, Database, Result),
    bson:doc_get(Result, ok, 1.0).
*/

:- end_tests('mongo:command/3').
