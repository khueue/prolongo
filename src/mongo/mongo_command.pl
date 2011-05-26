:- module(mongo_command,
    [
        list_commands/2,
        list_collection_names/2
    ]).

/** <module> xxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).

command_collection('$cmd').

doc_ok(Doc) :-
    bson:doc_get(Doc, ok, Value),
    doc_ok_value(Value).

% XXX Which of these are actually required?
doc_ok_value(1.0).
doc_ok_value(1).
doc_ok_value(+true).

/*
command(Database, Command, Result) :-
    command_namespace(CommandNamespace),
    command(Mongo, CommandNamespace, Command, Result).

command(Mongo, Coll, Command, Docs) :-
    mongo_get_database(Mongo, Database),
    namespace(Database, Coll, Namespace),
    build_command_message(Namespace, Command, Message),
    send_to_server(Mongo, Message),
    read_reply(Mongo, _Header, _Info, Docs).

drop_collection(Mongo, Collection, Result) :-
    Command = [drop-Collection],
    command(Mongo, Command, Result).

drop_database(Mongo, Database, Result) :-
    Command = [dropDatabase-1],
    use_database(Mongo, Database, Mongo1),
    command(Mongo1, Command, Result).

list_database_infos(Mongo, DatabaseInfos) :-
    Command = [listDatabases-1],
    use_database(Mongo, admin, Mongo1),
    command(Mongo1, Command, Result),
    bson:doc_get(Result, databases, DatabaseInfoArray),
    repack_database_infos(DatabaseInfoArray, DatabaseInfos).

repack_database_infos([], []).
repack_database_infos([[name-Name|Info]|Infos], [Name-Info|Names]) :-
    repack_database_infos(Infos, Names).

list_database_names(Mongo, DatabaseNames) :-
    list_database_infos(Mongo, DatabaseInfos),
    bson:doc_keys(DatabaseInfos, DatabaseNames).
*/

%%  list_commands.
%
%   xxxxxxxxxx

list_commands(Database, Result) :-
    command_collection(CollectionName),
    mongo_database:get_collection(Database, CollectionName, Collection),
    mongo_find:find_one(Collection, [listCommands-1], [commands-1], Result).

%%  list_collection_names.
%
%   xxxxxxxxxxxxxxx

list_collection_names(Database, Names) :-
    mongo_database:get_collection(Database, 'system.namespaces', Collection),
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
