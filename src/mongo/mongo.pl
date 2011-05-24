:- module(mongo,
    [
        % xxx missing exports
    ]).

/** <module> MongoDB driver.
 *
 *  Provides connection management and wraps the MongoDB API.
 *
 *  @see <http://www.mongodb.org/>
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_defaults), []).
:- use_module(mongo(mongo_bytes), []).
:- use_module(mongo(mongo_connection), []).
:- use_module(mongo(mongo_collection), []).
:- use_module(mongo(mongo_database), []).
:- use_module(mongo(mongo_delete), []).
:- use_module(mongo(mongo_update), []).
:- use_module(mongo(mongo_cursor), []).
:- use_module(mongo(mongo_find), []).
:- use_module(mongo(mongo_insert), []).
:- use_module(mongo(mongo_util), []).

command_namespace('$cmd').

doc_ok(Doc) :-
    bson:doc_get(Doc, ok, Value),
    doc_ok_value(Value).

% XXX Which of these are actually required?
doc_ok_value(1.0).
doc_ok_value(1).
doc_ok_value(+true).

list_collection_names(Mongo, Names) :-
    Command = [],
    mongo:command(Mongo, 'system.namespaces', Command, Result),
    repack_collection_names(Result, Names).

repack_collection_names([], []).
repack_collection_names([[name-Name]|Pairs], [Name|Names]) :-
    repack_collection_names(Pairs, Names).

drop_collection(Mongo, Collection, Result) :-
    Command = [drop-Collection],
    command(Mongo, Command, Result).

drop_database(Mongo, Database, Result) :-
    Command = [dropDatabase-1],
    use_database(Mongo, Database, Mongo1),
    command(Mongo1, Command, Result).

list_commands(Mongo, Result) :-
    Command = [listCommands-1],
    command(Mongo, Command, Result).

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

command(Mongo, Command, Result) :-
    command_namespace(CommandNamespace),
    command(Mongo, CommandNamespace, Command, Result).

command(Mongo, Coll, Command, Docs) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Coll, FullCollName),
    build_command_message(FullCollName, Command, Message),
    send_to_server(Mongo, Message),
    read_reply(Mongo, _Header, _Info, Docs).

build_command_message(FullCollName, Document, Bytes) :-
    phrase(c_string(FullCollName), BytesFullCollName),
    bson:doc_bytes(Document, BytesDocument),
    phrase(build_command_message_aux(
        BytesFullCollName, BytesDocument, BytesLength),
        Bytes),
    lists:length(Bytes, Length),
    int32crap(Length, BytesLength).

build_command_message_aux(BytesFullCollName, BytesCommand, BytesLength) -->
    { BytesLength = [_,_,_,_] },
    BytesLength, % Message length.
    [124,  0,  0,  0], %
    [  0,  0,  0,  0], %
    [212,  7,  0,  0], % 2004: query
    [  0,  0,  0,  0], % flags
    BytesFullCollName,
    [  0,  0,  0,  0], % num skip
    [  2,  0,  0,  0], % num return xxxxxxxxxxxxxxxxxxxxxxx
    BytesCommand.
