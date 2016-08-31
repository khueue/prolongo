/** <module> Helpers for dealing with the test database.
 */

:- module(_, [
    database_name/1,
    collection_name/1,
    database/2,
    drop_all_test_databases/0,
    collection/2,
    up/2,
    down/1,
    create_n_docs/2
]).

:- include(include/common).

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
    mongo_util:ms_since_epoch(Millis),
    core:atomic_list_concat([prolongo_test_suite,Millis], '-', DbName),
    assert(asserted_database_name(DbName)).

collection_name(testcoll).

database(Conn, Db) :-
    database_name(DbName),
    mongo:get_database(Conn, DbName, Db).

collection(Conn, Coll) :-
    database(Conn, Db),
    collection_name(CollName),
    mongo:get_collection(Db, CollName, Coll).

drop_all_test_databases :-
    mongo:new_connection(Conn),
    mongo:list_database_names(Conn, DbNames),
    drop_all_test_databases(DbNames, Conn),
    mongo:free_connection(Conn).

drop_all_test_databases([], _Conn).
drop_all_test_databases([DbName|DbNames], Conn) :-
    core:atomic_list_concat(['prolongo_test_suite'|_], '-', DbName),
    !,
    mongo:get_database(Conn, DbName, Db),
    mongo:drop_database(Db),
    drop_all_test_databases(DbNames, Conn).
drop_all_test_databases([_NotTestDbName|DbNames], Conn) :-
    drop_all_test_databases(DbNames, Conn).

up(Conn, Coll) :-
    mongo:new_connection(Conn),
    collection(Conn, Coll).

down(Conn) :-
    mongo:free_connection(Conn).

create_n_docs(0, []) :- !.
create_n_docs(N, [[hello-world,number-N]|Docs]) :-
    N1 is N - 1,
    create_n_docs(N1, Docs).
