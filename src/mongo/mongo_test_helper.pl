:- module(mongo_test_helper,
    [
        database_name/1,
        collection_name/1,
        database/2,
        drop_database/0,
        collection/2
    ]).

/** <module> Various helpers for dealing with the test database.
 */

:- include(misc(common)).

:- use_module(mongo(mongo), []).
:- use_module(misc(util), []).

:- dynamic asserted_database_name/1.

%%  database_name(+DbName) is det.
%
%   True if DbName is a unique name to be used for the test database
%   during this session. This is useful to avoid collisions and to make
%   it possible to run several test runs in parallel.

database_name(DbName) :-
    asserted_database_name(DbName),
    !.
database_name(DbName) :-
    util:ms_since_epoch(Millis),
    core:atomic_list_concat([prolongo_test_suite,Millis], '_', DbName),
    assert(asserted_database_name(DbName)).

collection_name(testcoll).

database(Conn, Db) :-
    database_name(DbName),
    mongo:get_database(Conn, DbName, Db).

collection(Conn, Coll) :-
    database(Conn, Db),
    collection_name(CollName),
    mongo:get_collection(Db, CollName, Coll).

drop_database :-
    mongo:new_connection(Conn),
    database(Conn, Db),
    mongo:drop_database(Db),
    mongo:free_connection(Conn).
