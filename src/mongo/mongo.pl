:- module(mongo,
    [
        new_mongo/1,
        new_mongo/3,
        free_mongo/1
    ]).

% <module> MongoDB driver.

:- include(misc(common)).

:- use_module(bson(bson)).

mongo_default_host(localhost).
mongo_default_port(27017).

new_mongo(Mongo) :-
    mongo_default_host(Host),
    mongo_default_port(Port),
    new_mongo(Host, Port, Mongo).

new_mongo(Host, Port, Mongo) :-
    Mongo = mongo(Read,Write),
    setup_call_catcher_cleanup(
        socket:tcp_socket(Socket),
        socket:tcp_connect(Socket, Host:Port),
        exception(_),
        socket:tcp_close_socket(Socket)),
    %call_cleanup(
    socket:tcp_open_socket(Socket, Read, Write).
        %free_mongo(Mongo)). % Do something on fail to open.

free_mongo(mongo(Read,Write)) :-
    core:close(Read, [force(true)]),
    core:close(Write, [force(true)]).

send_bytes(Bytes, Write) :-
    core:format(Write, '~s', [Bytes]).

tryit :-
    % prepend mess length 4 bytes! ...
    Message0 =
    [
        123,0,0,0,
        0,0,0,0,
        210,7,0,0,       % 2002 : op insert

        0,0,0,0,
        115,97,109,112,108,101,95,97,112,112,95,
            100,101,118,101,108,111,112,109,101,
            110,116,46,117,115,101,114,115,0 % sample_app_development.users\0
    ], % ... followed by 0+ docs.
    bson:term_bson([hello: utc(0)], Doc),
    append(Message0, Doc, Message1),
    length(Message1, LenWithout4),
    RealLen is LenWithout4 + 4,
    bson_bits:integer_bytes(RealLen, 4, little, BytesForLen),
    append(BytesForLen, Message1, Message),
    Mongo = mongo(Read,Write),
    new_mongo(Mongo),
    send_bytes(Message, Write),
    core:flush_output(Write),
    /*
    read_response(Read, Bytes),
    format('Response bytes: ~w~n', [Bytes]),
    parse_response_header(Bytes, [Len,ReqId,RespTo,OpCode]),
    format('Len: ~w~n', [Len]),
    format('ReqId: ~w~n', [ReqId]),
    format('RespTo: ~w~n', [RespTo]),
    format('OpCode: ~w~n', [OpCode]),*/
    free_mongo(Mongo).

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

read_response(Read, [B0,B1,B2,B3|Bytes]) :-
    get_byte(Read, B0),
    get_byte(Read, B1),
    get_byte(Read, B2),
    get_byte(Read, B3),
    BytesForLen = [B0,B1,B2,B3],
    bson_bits:integer_bytes(Len, 4, little, BytesForLen),
    Len4 is Len - 4,
    read_n_bytes(Read, Len4, Bytes).

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

length4(Len, Bytes) :-
    bson_bits:integer_bytes(Len, 4, little, Bytes).

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
