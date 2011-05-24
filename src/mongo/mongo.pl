:- module(mongo,
    [
        new_mongo/1,
        new_mongo/3,
        free_mongo/1,
        find_one/4,
        find_one/5
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

command_namespace('$cmd').

% xxx new:
delete(Coll, Selector) :-
    collection_get_namespace(Coll, FullCollName),
    phrase(build_delete_bytes(FullCollName, Selector), BytesSend),
    count_bytes_and_set_length(BytesSend),
    collection_get_connection(Coll, Conn),
    send_to_server(Conn, BytesSend).

% old: xxx
delete(Mongo, Collection, Selector) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Collection, FullCollName),
    phrase(build_delete_bytes(FullCollName, Selector), BytesSend),
    count_bytes_and_set_length(BytesSend),
    send_to_server(Mongo, BytesSend).

build_delete_bytes(FullCollName, Selector) -->
    build_header(45678, 45678, 2006),
    int32(0), % ZERO.
    c_string(FullCollName),
    int32(0), % Flags.
    build_bson_doc(Selector).

%%%%%%%%%%%%%%%%%%%

update(Coll, Selector, Modifier) :-
    update(Coll, Selector, Modifier, []).

update(Coll, Selector, Modifier, Options) :-
    collection_get_namespace(Coll, FullCollName),
    update_options_value(Options, Flags),
    phrase(build_update_bytes(FullCollName, Selector, Modifier, Flags), BytesSend),
    count_bytes_and_set_length(BytesSend),
    collection_get_connection(Coll, Conn),
    send_to_server(Conn, BytesSend).

% old:
update(Mongo, Coll, Selector, Modifier) :-
    update(Mongo, Coll, Selector, Modifier, []).

update(Mongo, Coll, Selector, Modifier, Options) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Coll, FullCollName),
    update_options_value(Options, Flags),
    phrase(build_update_bytes(FullCollName, Selector, Modifier, Flags), BytesSend),
    count_bytes_and_set_length(BytesSend),
    send_to_server(Mongo, BytesSend).

build_update_bytes(FullCollName, Selector, Modifier, Flags) -->
    build_header(765, 765, 2001),
    int32(0), % ZERO.
    c_string(FullCollName),
    int32(Flags),
    build_bson_doc(Selector),
    build_bson_doc(Modifier).

% Not very sophisticated, but dead simple.
update_options_value([upsert,multi], 3) :- !.
update_options_value([multi,upsert], 3) :- !.
update_options_value([upsert],       1) :- !.
update_options_value([multi],        2) :- !.
update_options_value([],             0) :- !.

%%%%%%%%%%%%%%%%%%

find_one(Mongo, Collection, Query, Result) :-
    find_one(Mongo, Collection, Query, [], Result).

find_one(Mongo, Collection, Query, ReturnFields, Result) :-
    find(Mongo, Collection, Query, ReturnFields, 0, 1, _Cursor, Docs),
    package_result_doc(Docs, Result).

package_result_doc([], nil).
package_result_doc([Doc], Doc).

find_all(Mongo, Coll, Query, ReturnFields, Docs) :-
    phrase(find_all(Mongo, Coll, Query, ReturnFields), Docs).

% XXX Should be implemented using the "exhaust" flag.
find_all(Mongo, Coll, Query, ReturnFields) -->
    { mongo:find(Mongo, Coll, Query, ReturnFields, 0, 0, Cursor, Docs0) },
    Docs0,
    { mongo:cursor_exhaust(Cursor, DocsRest) },
    DocsRest.

find(Mongo, Coll, Query, ReturnFields,
  Skip, Limit, cursor(Mongo,Coll,CursorId), Docs)
  :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Coll, FullCollName),
    phrase(build_find_bytes(FullCollName, Query, ReturnFields, Skip, Limit), BytesFind),
    count_bytes_and_set_length(BytesFind),
    send_to_server(Mongo, BytesFind),
    read_reply(Mongo, _Header, info(_Flags,CursorId,_Start,_Num), Docs).

build_find_bytes(FullCollName, Query, ReturnFields, Skip, Limit) -->
    build_header(4567, 4567, 2004),
    int32(0), % Flags.
    c_string(FullCollName),
    int32(Skip),
    int32(Limit),
    build_bson_doc(Query),
    build_bson_doc(ReturnFields).

cursor_kill(cursor(Mongo,_Coll,CursorId)) :-
    message_cursor_kill(CursorId, BytesMessage),
    send_to_server(Mongo, BytesMessage).

message_cursor_kill(CursorId, BytesMessage) :-
    phrase(message_cursor_kill(CursorId), BytesMessage),
    count_bytes_and_set_length(BytesMessage).

message_cursor_kill(CursorId) -->
    build_header(4567, 4567, 2007),
    int32(0), % ZERO.
    int32(1), % Number of cursor IDs.
    int64(CursorId). % Cursor IDs.

cursor_get_more(cursor(Mongo,Coll,CursorId), Limit, Docs, cursor(Mongo,Coll,CursorId1)) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Coll, FullCollName),
    phrase(message_get_more(FullCollName, Limit, CursorId), BytesMessage),
    count_bytes_and_set_length(BytesMessage),
    send_to_server(Mongo, BytesMessage),
    read_reply(Mongo, _Header, info(_Flags,CursorId1,_Start,_Num), Docs).

message_get_more(FullCollName, Limit, CursorId) -->
    build_header(4567, 4567, 2005),
    int32(0), % ZERO.
    c_string(FullCollName),
    int32(Limit),
    int64(CursorId).

cursor_has_more(cursor(_Mongo,_Coll,CursorId)) :-
    CursorId \== 0.

cursor_exhaust(Cursor, Docs) :-
    phrase(cursor_exhaust(Cursor), Docs).

cursor_exhaust(Cursor) -->
    { cursor_has_more(Cursor) },
    !,
    % Fetching the default number of docs -- good/bad?
    { cursor_get_more(Cursor, 0, Docs, Cursor1) },
    Docs,
    cursor_exhaust(Cursor1).
cursor_exhaust(_Cursor) -->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_header(RequestId, ResponseTo, OpCode) -->
    [_,_,_,_], % Length of entire message.
    int32(RequestId),
    int32(ResponseTo),
    int32(OpCode).

build_bson_doc(Doc) -->
    { bson:doc_bytes(Doc, Bytes) },
    Bytes.

build_bson_docs(Docs) -->
    { bson:docs_bytes(Docs, Bytes) },
    Bytes.

%%  new_mongo(-Mongo) is semidet.
%%  new_mongo(-Mongo, +Host, +Port) is semidet.
%
%   True if Mongo represents an opaque handle to a new MongoDB
%   server connection. Host and Port may be supplied, otherwise
%   the defaults (see mongo_defaults) are used.

new_mongo(Mongo) :-
    mongo_defaults:host(Host),
    mongo_defaults:port(Port),
    new_mongo(Mongo, Host, Port).

new_mongo(Mongo, Host, Port) :-
    mongo(socket(Read,Write), '', Mongo),
    setup_call_catcher_cleanup(
        socket:tcp_socket(Socket),
        socket:tcp_connect(Socket, Host:Port),
        exception(_),
        socket:tcp_close_socket(Socket)),
    %call_cleanup(
    socket:tcp_open_socket(Socket, Read, Write).
    %free_mongo(Mongo)). % Do something on fail to open.

new_connection(Conn) :-
    new_mongo(Conn).

new_connection(Conn, Host, Port) :-
    new_mongo(Conn, Host, Port).

get_database(Conn, DbName, db(Conn,DbName)).

get_collection(Db, CollName, Coll) :-
    Db = db(_Conn,DbName),
    Coll = coll(Db,FullCollName),
    full_coll_name(DbName, CollName, FullCollName).

collection_get_namespace(coll(_Db,FullCollName), FullCollName).

collection_get_connection(coll(db(Conn,_DbName),_FullCollName), Conn).

%%  free_mongo(+Mongo) is det.
%
%   Frees any resources associated with the Mongo handle,
%   rendering it unusable.

free_mongo(Mongo) :-
    mongo_socket_read(Mongo, Read),
    mongo_socket_write(Mongo, Write),
    core:close(Read, [force(true)]),
    core:close(Write, [force(true)]).

count_bytes_and_set_length(Bytes) :-
    Bytes = [L0,L1,L2,L3|_],
    lists:length(Bytes, Length),
    bson_bits:integer_bytes(Length, 4, little, [L0,L1,L2,L3]).

read_from_server(Mongo, Bytes) :-
    mongo_socket_read(Mongo, Read),
    read_response_bytes(Read, Bytes).

send_to_server(Mongo, Bytes) :-
    mongo_socket_write(Mongo, Write),
    send_bytes_and_flush(Bytes, Write).

use_database(Mongo, Database, Mongo1) :-
    mongo_set_database(Mongo, Database, Mongo1).

mongo_set_database(Mongo, Database, Mongo1) :-
    util:set_arg(Mongo, 2, Database, Mongo1).

mongo_get_database(Mongo, Database) :-
    util:get_arg(Mongo, 2, Database).

% Constructor.
mongo(Socket, Database, Mongo) :-
    Mongo = mongo(Socket,Database).

mongo_socket(Mongo, Socket) :-
    util:get_arg(Mongo, 1, Socket).

mongo_socket_read(Mongo, Read) :-
    mongo_socket(Mongo, Socket),
    util:get_arg(Socket, 1, Read).

mongo_socket_write(Mongo, Write) :-
    mongo_socket(Mongo, Socket),
    util:get_arg(Socket, 2, Write).

doc_ok(Doc) :-
    bson:doc_get(Doc, ok, Value),
    doc_ok_value(Value).

% XXX Which of these are actually required?
doc_ok_value(1.0).
doc_ok_value(1).
doc_ok_value(+true).

send_bytes_and_flush(Bytes, Write) :-
    send_bytes(Bytes, Write),
    core:flush_output(Write).

send_bytes(Bytes, Write) :-
    core:format(Write, '~s', [Bytes]).

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

insert_batch(Mongo, Collection, Options, Docs) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Collection, FullCollName),
    insert_batch_flags(Options, Flags),
    phrase(build_insert_batch_bytes(FullCollName, Flags, Docs), BytesSend),
    count_bytes_and_set_length(BytesSend),
    send_to_server(Mongo, BytesSend).

insert_batch_flags([keep_going], 1) :- !.
insert_batch_flags([],           0) :- !.

build_insert_batch_bytes(FullCollName, Flags, Docs) -->
    build_header(45678, 45678, 2002),
    int32(Flags),
    c_string(FullCollName),
    build_bson_docs(Docs).

insert(Mongo, Collection, Doc) :-
    insert_batch(Mongo, Collection, [], [Doc]).

full_coll_name(Database, Collection, FullCollName) :-
    core:atomic_list_concat([Database,Collection], '.', FullCollName).

c_string(Atom) -->
    { bson_unicode:utf8_bytes(Atom, Bytes) },
    Bytes,
    [0].

int32crap(Len, Bytes) :-
    bson_bits:integer_bytes(Len, 4, little, Bytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_reply(Mongo, Header, Info, Docs) :-
    read_from_server(Mongo, Bytes),
    parse_response(Bytes, Header, Info, Docs).

read_response_bytes(Read, [B0,B1,B2,B3|Bytes]) :-
    read_n_bytes(Read, 4, BytesForLen),
    BytesForLen = [B0,B1,B2,B3],
    bson_bits:integer_bytes(Len, 4, little, BytesForLen),
    LenBut4 is Len - 4,
    read_n_bytes(Read, LenBut4, Bytes).

read_n_bytes(_Read, 0, []) :- !.
read_n_bytes(Read, N, [Byte|Bytes]) :-
    core:get_byte(Read, Byte),
    N1 is N - 1,
    read_n_bytes(Read, N1, Bytes).

parse_response(Bytes, Header, Info, Docs) :-
    % xxx inspect_response_bytes(Bytes),
    phrase(parse_response_metadata(Header, Info), Bytes, RestBytes),
    bson:docs_bytes(Docs, RestBytes).

parse_response_metadata(Header, Info) -->
    { Header = header(MessageLen,RequestId,ResponseTo,OpCode) },
    { Info = info(Flags,CursorId,StartFrom,NumReturn) },
    int32(MessageLen),
    int32(RequestId),
    int32(ResponseTo),
    int32(OpCode),
    int32(Flags),
    int64(CursorId),
    int32(StartFrom),
    int32(NumReturn).

int32(Int) -->
    [L0,L1,L2,L3],
    { bson_bits:integer_bytes(Int, 4, little, [L0,L1,L2,L3]) }.

int64(Int) -->
    [L0,L1,L2,L3,L4,L5,L6,L7],
    { bson_bits:integer_bytes(Int, 8, little, [L0,L1,L2,L3,L4,L5,L6,L7]) }.

%%%%%%%%%%%% xxx debug:

inspect_response_bytes(Bytes) :-
    core:format('~n--- Begin Response ---~n'),
    phrase(inspect_response_paperwork, Bytes, Rest),
    bson:docs_bytes(Docs, Rest),
    inspect_response_docs(Docs),
    core:format('--- End Response ---~n~n').

inspect_response_paperwork -->
    int32(MessLen),
    { core:format('MessLen: ~p~n', [MessLen]) },
    int32(RequestId),
    { core:format('RequestId: ~p~n', [RequestId]) },
    int32(ResponseTo),
    { core:format('ResponseTo: ~p~n', [ResponseTo]) },
    int32(OpCode),
    { core:format('OpCode: ~p~n', [OpCode]) },
    int32(ResponseFlags),
    { core:format('ResponseFlags: ~p~n', [ResponseFlags]) },
    int64(CursorId),
    { core:format('CursorId: ~p~n', [CursorId]) },
    int32(StartingFrom),
    { core:format('StartingFrom: ~p~n', [StartingFrom]) },
    int32(NumberReturned),
    { core:format('NumberReturned: ~p~n', [NumberReturned]) }.

inspect_response_docs([]).
inspect_response_docs([Doc|Docs]) :-
    bson_format:pp(Doc, 1, '  '), nl,
    inspect_response_docs(Docs).
