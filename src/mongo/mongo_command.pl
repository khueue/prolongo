:- module(mongo_command,
    [
        command/3,
        list_commands/2,
        list_collection_names/2,
        list_database_infos/2,
        list_database_names/2,
        drop_collection/1,
        drop_database/1
    ]).

/** <module> xxxxxxx
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

%%  command.
%
%   xxxxxxxxxx

command(Database, Query, Doc) :-
    database_cmd_collection(Database, CmdColl),
    mongo_find:find_one(CmdColl, Query, [], Doc).

%%  drop_database.
%
%   xxxxxxxxx

drop_database(Database) :-
    database_cmd_collection(Database, CmdColl),
    mongo_find:find_one(CmdColl, [dropDatabase-1], [], Doc),
    doc_ok(Doc),
    !.
drop_database(Database) :-
    mongo_database:database_name(Database, DatabaseName),
    throw(mongo_error('could not drop database', DatabaseName)).

%%  drop_collection.
%
%   xxxxxxxxx

drop_collection(Collection) :-
    mongo_collection:collection_database(Collection, Database),
    database_cmd_collection(Database, CmdColl),
    mongo_collection:collection_name(Collection, CollectionName),
    mongo_find:find_one(CmdColl, [drop-CollectionName], [], Doc),
    doc_ok(Doc),
    !.
drop_collection(Collection) :-
    mongo_collection:collection_name(Collection, CollectionName),
    throw(mongo_error('could not drop collection', CollectionName)).

%%  list_database_names.
%
%   xxxxxxxxx

list_database_names(Connection, Names) :-
    list_database_infos(Connection, Infos),
    bson:doc_keys(Infos, Names).

%%  list_database_infos.
%
%   xxxxxxxxxx

list_database_infos(Connection, Infos) :-
    admin_database(DatabaseName),
    mongo_connection:get_database(Connection, DatabaseName, Database),
    database_cmd_collection(Database, CmdColl),
    mongo_find:find_one(CmdColl, [listDatabases-1], [], Doc),
    bson:doc_get(Doc, databases, InfoArray),
    repack_database_infos(InfoArray, Infos).

repack_database_infos([], []).
repack_database_infos([[name-Name|Info]|Infos], [Name-Info|Names]) :-
    repack_database_infos(Infos, Names).

%%  list_commands.
%
%   xxxxxxxxxx

list_commands(Database, Result) :-
    database_cmd_collection(Database, CmdColl),
    mongo_find:find_one(CmdColl, [listCommands-1], [commands-1], Result).

%%  list_collection_names.
%
%   xxxxxxxxxxxxxxx

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
    core:atomic_list_concat([_|L], '.', Namespace),
    core:atomic_list_concat(L, '.', Name).

%%  doc_ok.
%
%   xxxxxxxxxxx

doc_ok(Doc) :-
    bson:doc_get_strict(Doc, ok, Value),
    doc_ok_value(Value).

% XXX Which of these are actually required?
doc_ok_value(1.0).
doc_ok_value(1).
doc_ok_value(+true).
