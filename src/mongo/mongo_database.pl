:- module(mongo_database,
    [
        new_database/3,
        database_connection/2,
        database_name/2,
        get_collection/3
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_util), []).

new_database(Connection, DatabaseName, Database) :-
    Database = database(Connection,DatabaseName).

database_connection(Database, Connection) :-
    util:get_arg(Database, 1, Connection).

database_name(Database, DatabaseName) :-
    util:get_arg(Database, 2, DatabaseName).

%%  get_collection(+Database, +CollectionName, -Collection) is det.
%
%   Collection is a handle to the collection CollectionName in Database.
%   No communication is performed so the actual collection might or might
%   not exist.

get_collection(Database, CollectionName, Collection) :-
    mongo_collection:new_collection(Database, CollectionName, Collection).
