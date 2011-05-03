:- module(mongo,
    [
        new_mongo/1,
        new_mongo/3,
        free_mongo/1
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

% Defaults.
mongo_default_host(localhost).
mongo_default_port(27017).

command_namespace('$cmd').

%%  new_mongo(-Mongo) is semidet.
%%  new_mongo(-Mongo, +Host, +Port) is semidet.
%
%   True if Mongo represents an opaque handle to a new MongoDB
%   server connection. Host and Port may be supplied, otherwise
%   the defaults (host localhost and port 27017) are used.

new_mongo(Mongo) :-
    mongo_default_host(Host),
    mongo_default_port(Port),
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

%%  free_mongo(+Mongo) is det.
%
%   Frees any resources associated with the Mongo handle,
%   rendering it unusable.

free_mongo(Mongo) :-
    mongo_socket_read(Mongo, Read),
    mongo_socket_write(Mongo, Write),
    core:close(Read, [force(true)]),
    core:close(Write, [force(true)]).

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
    bson:doc_values(Result, Names).

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

command(Mongo, Collection, Command, Result) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Collection, FullCollName),
    build_command_message(FullCollName, Command, Message),
    mongo_socket_write(Mongo, Write),
    send_bytes_and_flush(Message, Write),
    mongo_socket_read(Mongo, Read),
    read_response_bytes(Read, Bytes),
    phrase(parse_response_header(header(Len,ReqId,RespTo,OpCode)), Bytes, Bytes1),
    skip_n(Bytes1, 20, Bytes2),
    bson:doc_bytes(Result, Bytes2).

build_command_message(FullCollName, Document, Bytes) :-
    phrase(c_string(FullCollName), BytesFullCollName),
    bson:doc_bytes(Document, BytesDocument),
    phrase(build_command_message_aux(
        BytesFullCollName, BytesDocument, BytesLength),
        Bytes),
    lists:length(Bytes, Length),
    length4(Length, BytesLength).

build_command_message_aux(BytesFullCollName, BytesCommand, BytesLength) -->
    { BytesLength = [_,_,_,_] },
    BytesLength, % Message length.
    [124,  0,  0,  0], %
    [  0,  0,  0,  0], %
    [212,  7,  0,  0], % 2004: query
    [  0,  0,  0,  0], % flags
    BytesFullCollName,
    [  0,  0,  0,  0], % num skip
    [  1,  0,  0,  0], % num return
    BytesCommand.

full_coll_name(Database, Collection, FullCollName) :-
    core:atomic_list_concat([Database,Collection], '.', FullCollName).

insert(Mongo, Collection, Document) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Collection, FullCollName),
    build_insert_message(FullCollName, Document, Message),
    mongo_socket_write(Mongo, Write),
    send_bytes_and_flush(Message, Write).

build_insert_message(FullCollName, Document, Bytes) :-
    phrase(c_string(FullCollName), BytesFullCollName),
    bson:doc_bytes(Document, BytesDocument),
    phrase(build_insert_message_aux(
        BytesFullCollName, BytesDocument, BytesLength),
        Bytes),
    lists:length(Bytes, Length),
    length4(Length, BytesLength).

build_insert_message_aux(BytesFullCollName, BytesDocument, BytesLength) -->
    { BytesLength = [_,_,_,_] },
    BytesLength,       % Message length.
    [123,  0,  0,  0], %
    [  0,  0,  0,  0], %
    [210,  7,  0,  0], % 2002: insert
    % Stuff.
    [  0,  0,  0,  0], % ZERO
    % Interesting stuff.
    BytesFullCollName,
    BytesDocument.

c_string(Atom) -->
    { bson_unicode:utf8_bytes(Atom, Bytes) },
    Bytes,
    [0].

length4(Len, Bytes) :-
    bson_bits:integer_bytes(Len, 4, little, Bytes).

int32(Len, Bytes) :-
    bson_bits:integer_bytes(Len, 4, little, Bytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip_n(L, 0, L) :- !.
skip_n([_|L], N, L2) :-
    N1 is N - 1,
    skip_n(L, N1, L2).

parse_response_header(header(Len,ReqId,RespTo,OpCode)) -->
    [L0,L1,L2,L3],
    [Req0,Req1,Req2,Req3],
    [Resp0,Resp1,Resp2,Resp3],
    [Op0,Op1,Op2,Op3],
    { int32(Len, [L0,L1,L2,L3]) },
    { int32(ReqId, [Req0,Req1,Req2,Req3]) },
    { int32(RespTo, [Resp0,Resp1,Resp2,Resp3]) },
    { int32(OpCode, [Op0,Op1,Op2,Op3]) }.

read_response_bytes(Read, [B0,B1,B2,B3|Bytes]) :-
    read_n_bytes(Read, 4, BytesForLen),
    BytesForLen = [B0,B1,B2,B3],
    length4(Len, BytesForLen),
    LenBut4 is Len - 4,
    read_n_bytes(Read, LenBut4, Bytes).

read_n_bytes(_Read, 0, []) :- !.
read_n_bytes(Read, N, [Byte|Bytes]) :-
    get_byte(Read, Byte),
    N1 is N - 1,
    read_n_bytes(Read, N1, Bytes).

/*

struct MsgHeader {
    int32   messageLength; // total message size, including this
    int32   requestID;     // identifier for this message
    int32   responseTo;    // requestID from the original request
                           //   (used in reponses from db)
    int32   opCode;        // request type - see table below
}

struct OP_QUERY {
    MsgHeader header;                // standard message header
    int32     flags;                  // bit vector of query options. See ...
    cstring   fullCollectionName;    // "dbname.collectionname"
    int32     numberToSkip;          // number of documents to skip
    int32     numberToReturn;        // number of documents to return
                                     //  in the first OP_REPLY batch
    document  query;                 // query object.  See below for details.
  [ document  returnFieldSelector; ] // Optional. Selector indicating the fields
                                     //  to return.  See below for details.
}

*/
