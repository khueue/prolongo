:- module(mongo,
    [
        new_mongo/1,
        new_mongo/3,
        free_mongo/1
    ]).

/** <module> MongoDB driver.
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(bson(bson_format), []). % Temp XXX.

% Defaults.
mongo_default_host(localhost).
mongo_default_port(27017).

%%  new_mongo(-Mongo) is semidet.
%%  new_mongo(-Mongo, +Host, +Port) is semidet.
%
%   True if Mongo represents a handle to a new MongoDB server
%   connection. Host and Port may be supplied, otherwise the
%   defaults (host localhost and port 27017) are used.

new_mongo(Mongo) :-
    mongo_default_host(Host),
    mongo_default_port(Port),
    new_mongo(Mongo, Host, Port).

new_mongo(Mongo, Host, Port) :-
    Mongo = mongo(socket(Read,Write)),
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

% Mongo connection structure:
% mongo(socket(Read,Write))

mongo_socket(Mongo, Socket) :-
    core:arg(1, Mongo, Socket).

mongo_socket_read(Mongo, Read) :-
    mongo_socket(Mongo, Socket),
    core:arg(1, Socket, Read).

mongo_socket_write(Mongo, Write) :-
    mongo_socket(Mongo, Socket),
    core:arg(2, Socket, Write).

send_bytes_and_flush(Bytes, Write) :-
    send_bytes(Bytes, Write),
    core:flush_output(Write).

send_bytes(Bytes, Write) :-
    core:format(Write, '~s', [Bytes]).

command(Mongo, Command, Database, Result) :-
    core:atom_concat(Database, '.$cmd', DbCollection),
    c_string(DbCollection, DbCollectionBytes),
    bson:pairs_bson(Command, BsonCommand),
    lists:append(
        [
            [
                 L0, L1, L2, L3, % Message length.
                124,  0,  0,  0, %
                  0,  0,  0,  0, %
                212,  7,  0,  0  % 2004: query
            ],
            [0,0,0,0], % flags
            DbCollectionBytes,
            [0,0,0,0], % num skip
            [1,0,0,0], % num return
            BsonCommand
        ],
        Message),
    lists:length(Message, MessageLen),
    length4(MessageLen, [L0,L1,L2,L3]),
    mongo_socket_write(Mongo, Write),
    send_bytes_and_flush(Message, Write),
    mongo_socket_read(Mongo, Read),
    read_response(Read, Bytes),
    skip_n(Bytes, 36, Bytes1),
    bson:pairs_bson(Result, Bytes1).

insert(Mongo, Document, FullCollName) :-
    c_string(FullCollName, FullCollNameBytes),
    bson:pairs_bson(Document, BsonDocument),
    lists:append(
        [
            [
                 L0, L1, L2, L3, % Message length.
                123,  0,  0,  0, %
                  0,  0,  0,  0, %
                210,  7,  0,  0  % 2002: insert
            ],
            [0,0,0,0], % ZERO
            FullCollNameBytes,
            BsonDocument
        ],
        Message),
    lists:length(Message, MessageLen),
    length4(MessageLen, [L0,L1,L2,L3]),
    mongo_socket_write(Mongo, Write),
    send_bytes_and_flush(Message, Write).

c_string(Atom, Bytes) :-
    bson_unicode:utf8_bytes(Atom, Bytes0),
    lists:append(Bytes0, [0], Bytes).

length4(Len, Bytes) :-
    bson_bits:integer_bytes(Len, 4, little, Bytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip_n(L, 0, L) :- !.
skip_n([_|L], N, L2) :-
    N1 is N - 1,
    skip_n(L, N1, L2).

read_response(Read, [B0,B1,B2,B3|Bytes]) :-
    read_n_bytes(Read, 4, BytesForLen),
    BytesForLen = [B0,B1,B2,B3],
    length4(Len, BytesForLen),
    LenBut4 is Len - 4,
    read_n_bytes(Read, LenBut4, Bytes).

parse_response_header(Bytes, [Len,ReqId,RespTo,OpCode]) :-
    Bytes = [
        L0,L1,L2,L3,
        Req0,Req1,Req2,Req3,
        Res0,Res1,Res2,Res3,
        Op0,Op1,Op2,Op3
        |_Rest],
    length4(Len, [L0,L1,L2,L3]),
    length4(ReqId, [Req0,Req1,Req2,Req3]),
    length4(RespTo, [Res0,Res1,Res2,Res3]),
    length4(OpCode, [Op0,Op1,Op2,Op3]).

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
    int32     flags;                  // bit vector of query options.  See below for details.
    cstring   fullCollectionName;    // "dbname.collectionname"
    int32     numberToSkip;          // number of documents to skip
    int32     numberToReturn;        // number of documents to return
                                     //  in the first OP_REPLY batch
    document  query;                 // query object.  See below for details.
  [ document  returnFieldSelector; ] // Optional. Selector indicating the fields
                                     //  to return.  See below for details.
}

*/

/*
tryit :-
    Message =
    [
        56,0,0,0,        % mess length
        123,0,0,0,
        0,0,0,0,
        212,7,0,0,       % 2004 : op query

        0,0,0,0,
        115,97,109,112,108,101,95,97,112,112,
            95,100,101,118,101,108,111,112,
            109,101,110,116, 0, % sample_app_development\0
        0,0,0,0,
        0,0,0,0,

        5,0,0,0,
        0
    ],
    Mongo = mongo(Read,Write),
    new_mongo(Mongo),
    send_bytes(Message, Write),
    core:flush_output(Write),
    read_response(Read, Bytes),
    format('Response bytes: ~w~n', [Bytes]),
    parse_response_header(Bytes, [Len,ReqId,RespTo,OpCode]),
    format('Len: ~w~n', [Len]),
    format('ReqId: ~w~n', [ReqId]),
    format('RespTo: ~w~n', [RespTo]),
    format('OpCode: ~w~n', [OpCode]),
    free_mongo(Mongo).
*/
