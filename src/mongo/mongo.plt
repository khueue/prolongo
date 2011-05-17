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

:- begin_tests('mongo:find_one/4,5').

/*
test('cursor', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    mongo:insert(Mongo, Collection, [hello-world,number-1]),
    mongo:insert(Mongo, Collection, [hello-world,number-2]),
    mongo:insert(Mongo, Collection, [hello-world,number-3]),
    mongo:insert(Mongo, Collection, [hello-world,number-4]),
    mongo:insert(Mongo, Collection, [hello-world,number-5]),
    mongo:find(Mongo, Collection, [hello-world], [], Cursor),
    fail.
*/

test('entire doc', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    Doc = [hello-world, number-42],
    mongo:insert(Mongo, Collection, Doc),
    mongo:find_one(Mongo, Collection, [hello-world], Doc1),
    bson:doc_get(Doc1, number, 42).

test('return fields selector', [setup(up(Mongo)),cleanup(down(Mongo))]) :-
    collection(Collection),
    Doc = [hello-world, number-42],
    mongo:insert(Mongo, Collection, Doc),
    mongo:find_one(Mongo, Collection, [hello-world], [number-1], Doc1),
    bson:doc_get(Doc1, '_id', object_id(_)),
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
    write(Result), nl,
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
