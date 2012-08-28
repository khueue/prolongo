/** <module> Database handling.
 */

:- module(_,
    [
        new_database/3,
        database_connection/2,
        database_name/2,
        get_collection/3
    ]).

:- include(misc(common)).

%%  new_database.
%
%   XXX

new_database(Connection, DatabaseName, Database) :-
    Database = database(Connection,DatabaseName).

%%  database_connection.
%
%   XXX

database_connection(Database, Connection) :-
    util:get_arg(Database, 1, Connection).

%%  database_name.
%
%   XXX

database_name(Database, DatabaseName) :-
    util:get_arg(Database, 2, DatabaseName).

%%  get_collection(+Database, +CollectionName, -Collection) is det.
%
%   Collection is a handle to the collection CollectionName in Database.
%   No communication is performed so the actual collection might or might
%   not exist.

get_collection(Database, CollectionName, Collection) :-
    mongo_collection:new_collection(Database, CollectionName, Collection).
