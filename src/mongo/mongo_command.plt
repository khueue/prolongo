:- include(misc(common)).

:- use_module(mongo_test_helper, []).
:- use_module(bson(bson), []).
:- use_module(misc(util), []).

:- begin_tests('mongo commands').

test('list commands', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:collection_database(Coll, Database),
    mongo:list_commands(Database, Doc),
    bson:doc_get_strict(Doc, commands, _).

test('list collection names', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:collection_database(Coll, Database),
    mongo:list_collection_names(Database, Names),
    lists:member('system.indexes', Names),
    lists:member('testcoll', Names),
    !. % Not interested in member choices.

test('list database infos', [
        setup(mongo_test_helper:up(Conn,_Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:list_database_infos(Conn, Infos),
    mongo_test_helper:database_name(DbName),
    bson:doc_get_strict(Infos, DbName, _).

test('list database names', [
        setup(mongo_test_helper:up(Conn,_Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:list_database_names(Conn, Names),
    mongo_test_helper:database_name(DbName),
    lists:member(DbName, Names),
    !. % Not interested in member choices.

test('drop collection', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:drop_collection(Coll).

test('drop database', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:collection_database(Coll, Database),
    mongo:drop_database(Database).

test('generic command', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:collection_database(Coll, Database),
    mongo:command(Database, [profile - -1], Doc),
    bson:doc_get_strict(Doc, ok, _).

test('command get last error', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:collection_database(Coll, Database),
    mongo:get_last_error(Database, Doc),
    bson:doc_get_strict(Doc, ok, _).

:- end_tests('mongo commands').
