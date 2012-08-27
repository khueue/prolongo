:- module(mongo_command,
    [
        command/3,
        list_commands/2,
        list_collection_names/2,
        list_database_infos/2,
        list_database_names/2,
        drop_collection/1,
        drop_database/1,
        get_last_error/2
    ]).

/** <module> Command handling.
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).

command_collection('$cmd').
namespace_collection('system.namespaces').
admin_database('admin').

database_cmd_collection(Database, CmdColl) :-
    command_collection(CmdCollName),
    mongo_database:get_collection(Database, CmdCollName, CmdColl).

%%  command(+Database, +Query, -Doc).
%
%   True if Doc is the response to Query, issued on Database.

command(Database, Query, Doc) :-
    database_cmd_collection(Database, CmdColl),
    mongo_find:find_one(CmdColl, Query, Doc).

%%  get_last_error(+Database, -Doc).
%
%   True if Doc is a document describing the status of the latest query.

get_last_error(Database, Doc) :-
    command(Database, [getlasterror-1], Doc).

%%  drop_database(+Database).
%
%   True if Database is dropped. Throws an exception if Database could
%   not be dropped.

drop_database(Database) :-
    command(Database, [dropDatabase-1], Doc),
    doc_ok(Doc),
    !.
drop_database(Database) :-
    mongo_database:database_name(Database, DatabaseName),
    throw(mongo_error('could not drop database', [DatabaseName])).

%%  drop_collection(+Collection).
%
%   True if Collection is dropped from its database. Throws an exception
%   if Collection could not be dropped.

drop_collection(Collection) :-
    mongo_collection:collection_database(Collection, Database),
    database_cmd_collection(Database, CmdColl),
    mongo_collection:collection_name(Collection, CollectionName),
    mongo_find:find_one(CmdColl, [drop-CollectionName], Doc),
    doc_ok(Doc),
    !.
drop_collection(Collection) :-
    mongo_collection:collection_name(Collection, CollectionName),
    throw(mongo_error('could not drop collection', [CollectionName])).

%%  list_database_names(+Connection, -Names).
%
%   True if Names is a list of names of all logical databases.

list_database_names(Connection, Names) :-
    list_database_infos(Connection, Infos),
    bson:doc_keys(Infos, Names).

%%  list_database_infos(+Connection, -Infos).
%
%   True if Infos is a list of documents detailing all logical databases.

list_database_infos(Connection, Infos) :-
    admin_database(DatabaseName),
    mongo_connection:get_database(Connection, DatabaseName, Database),
    database_cmd_collection(Database, CmdColl),
    mongo_find:find_one(CmdColl, [listDatabases-1], Doc),
    bson:doc_get(Doc, databases, InfoArray),
    repack_database_infos(InfoArray, Infos).

repack_database_infos([], []).
repack_database_infos([[name-Name|Info]|Infos], [Name-Info|Names]) :-
    repack_database_infos(Infos, Names).

%%  list_commands(+Database, -Commands).
%
%   True if Commands is a list of documents detailing the commands that
%   can be executed on Database.

list_commands(Database, Result) :-
    database_cmd_collection(Database, CmdColl),
    mongo_find:find_one(CmdColl, [listCommands-1], [commands-1], Result).

%%  list_collection_names(+Database, -Names).
%
%   True if Names is the list of collection names in Database.

list_collection_names(Database, Names) :-
    namespace_collection(CollNamespacesName),
    mongo_database:get_collection(Database, CollNamespacesName, Collection),
    mongo_find:find_all(Collection, [], [], CollectionInfos),
    repack_collection_names(CollectionInfos, Names).

repack_collection_names([], []).
repack_collection_names([Pair|Pairs], [Name|Names]) :-
    acceptable_collection(Pair),
    !,
    repack_collection(Pair, Name),
    repack_collection_names(Pairs, Names).
repack_collection_names([_Pair|Pairs], Names) :-
    repack_collection_names(Pairs, Names).

acceptable_collection([name-Namespace]) :-
    \+ util:atom_contains(Namespace, '$').
acceptable_collection([name-Namespace]) :-
    util:atom_contains(Namespace, '.oplog.$').

repack_collection([name-Namespace], Name) :-
    mongo_collection:collection_without_namespace(Namespace, Name).

%%  doc_ok(+Doc) is semidet.
%
%   True if Doc is marked as okay.

doc_ok(Doc) :-
    bson:doc_get_strict(Doc, ok, Value),
    doc_ok_value(Value).

doc_ok_value(1.0) :- !.
doc_ok_value(Unknown) :-
    throw(mongo_error('unknown document okay value', [Unknown])).
