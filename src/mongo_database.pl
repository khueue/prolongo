/** <module> Database handling.
 */

:- module(_, [
    new_database/3,
    database_connection/2,
    database_name/2,
    get_collection/3
]).

:- include(include/common).

%%  new_database(+Connection, +DatabaseName, -Database) is det.
%
%   True if Database is a handle to the database named DatabaseName
%   issued over Connection. No communication is performed, so the
%   database might or might not already exist.

new_database(Connection, DatabaseName, Database) :-
    Database = database(Connection,DatabaseName).

%%  database_connection(+Database, -Connection) is det.
%
%   True if Connection is the connection used to access Database.

database_connection(Database, Connection) :-
    mongo_util:get_nth1_arg(Database, 1, Connection).

%%  database_name(+Database, -DatabaseName) is det.
%
%   True if DatabaseName is the name of Database.

database_name(Database, DatabaseName) :-
    mongo_util:get_nth1_arg(Database, 2, DatabaseName).

%%  get_collection(+Database, +CollectionName, -Collection) is det.
%
%   True if Collection is a handle to the collection CollectionName in
%   Database. No communication is performed so the actual collection might
%   or might not exist.

get_collection(Database, CollectionName, Collection) :-
    mongo_collection:new_collection(Database, CollectionName, Collection).
