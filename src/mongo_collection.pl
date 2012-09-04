/** <module> Collection handling.
 */

:- module(_,
    [
        new_collection/3,
        collection_database/2,
        collection_name/2,
        collection_namespace/2,
        collection_connection/2
    ]).

:- include(include/common).

%%  new_collection(+Database, +CollectionName, -Collection) is det.
%
%   True if Collection is a handle to the collection named
%   CollectionName within Database. No communication is performed,
%   so the collection might or might not already exist.

new_collection(Database, CollectionName, Collection) :-
    mongo_database:database_name(Database, DatabaseName),
    namespace_atom(DatabaseName, CollectionName, Namespace),
    Collection = collection(Database,Namespace).

%%  collection_database(+Collection, -Database) is det.
%
%   True if Database is a handle to the database containing Collection.

collection_database(Collection, Database) :-
    mongo_util:get_nth1_arg(Collection, 1, Database).

%%  collection_name(+Collection, -CollectionName) is det.
%
%   True if CollectionName is the name of Collection.

collection_name(Collection, CollectionName) :-
    collection_namespace(Collection, Namespace),
    collection_without_namespace(Namespace, CollectionName).

collection_without_namespace(NamespaceCollection, Collection) :-
    namespace_parts(NamespaceCollection, [_Namespace|Rest]),
    namespace_parts(Collection, Rest).

namespace_parts(Atom, Parts) :-
    core:atomic_list_concat(Parts, '.', Atom).

%%  collection_namespace(+Collection, -Namespace) is det.
%
%   True if Namespace is the namespace name for Collection.

collection_namespace(Collection, Namespace) :-
    mongo_util:get_nth1_arg(Collection, 2, Namespace).

%%  collection_connection(+Collection, -Connection) is det.
%
%   True if Connection is a handle to the connection used to access
%   Collection.

collection_connection(Collection, Connection) :-
    collection_database(Collection, Database),
    mongo_database:database_connection(Database, Connection).

namespace_atom(DatabaseName, CollectionName, Namespace) :-
    namespace_parts(Namespace, [DatabaseName,CollectionName]).
