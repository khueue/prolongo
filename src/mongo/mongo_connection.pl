:- module(mongo_connection,
    [
        new/1,
        new/3,
        free/1,
        get_database/3,
        send_to_server/2,
        read_reply/4
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_defaults), []).

%%  new_mongo(-Mongo) is semidet.
%%  new_mongo(-Mongo, +Host, +Port) is semidet.
%
%   xxx True if Mongo represents an opaque handle to a new MongoDB
%   server connection. Host and Port may be supplied, otherwise
%   the defaults (see mongo_defaults) are used.

new(Conn) :-
    mongo_defaults:host(Host),
    mongo_defaults:port(Port),
    new(Conn, Host, Port).

new(Conn, Host, Port) :-
    Conn = socket(ReadStream,WriteStream),
    setup_call_catcher_cleanup(
        socket:tcp_socket(Socket),
        socket:tcp_connect(Socket, Host:Port),
        exception(_),
        socket:tcp_close_socket(Socket)),
    %call_cleanup(
    socket:tcp_open_socket(Socket, ReadStream, WriteStream).
    %free_mongo(Mongo)). % Do something on fail to open.

%%  free(+Mongo) is det.
%
%   xxx Frees any resources associated with the Mongo handle,
%   rendering it unusable.

free(Conn) :-
    get_socket_read(Conn, ReadStream),
    get_socket_write(Conn, WriteStream),
    core:close(ReadStream, [force(true)]),
    core:close(WriteStream, [force(true)]).

%%  get_database.
%
%   xxxxx

get_database(Conn, DbName, database(Conn,DbName)).

% Socket.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_socket_read(Conn, ReadStream) :-
    get_socket(Conn, Socket),
    util:get_arg(Socket, 1, ReadStream).

get_socket_write(Conn, WriteStream) :-
    get_socket(Conn, Socket),
    util:get_arg(Socket, 2, WriteStream).

get_socket(Conn, Conn).

% Send to server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_to_server(Conn, Bytes) :-
    get_socket_write(Conn, WriteStream),
    send_bytes_and_flush(Bytes, WriteStream).

send_bytes_and_flush(Bytes, WriteStream) :-
    send_bytes(Bytes, WriteStream),
    core:flush_output(WriteStream).

send_bytes(Bytes, WriteStream) :-
    core:format(WriteStream, '~s', [Bytes]).

% Read from server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_reply(Mongo, Header, Info, Docs) :-
    read_from_server(Mongo, Bytes),
    parse_response(Bytes, Header, Info, Docs).

read_from_server(Conn, Bytes) :-
    get_socket_read(Conn, ReadStream),
    read_response_bytes(ReadStream, Bytes).

read_response_bytes(Read, [B0,B1,B2,B3|Bytes]) :-
    read_n_bytes(Read, 4, BytesForLen),
    BytesForLen = [B0,B1,B2,B3],
    bson_bits:integer_bytes(Len, 4, little, BytesForLen),
    LenBut4 is Len - 4,
    read_n_bytes(Read, LenBut4, Bytes).

read_n_bytes(_ReadStream, 0, []) :- !.
read_n_bytes(ReadStream, N, [Byte|Bytes]) :-
    core:get_byte(ReadStream, Byte),
    N1 is N - 1,
    read_n_bytes(ReadStream, N1, Bytes).

% Response parsing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_response(Bytes, Header, Info, Docs) :-
    % XXX inspect_response_bytes(Bytes),
    phrase(parse_response_meta(Header, Info), Bytes, RestBytes),
    parse_response_docs(RestBytes, Docs).

parse_response_meta(Header, Info) -->
    parse_response_header(Header),
    parse_response_info(Info).

parse_response_header(Header) -->
    { Header = header(MessageLength,RequestId,ResponseTo,OpCode) },
    mongo_bytes:int32(MessageLength),
    mongo_bytes:int32(RequestId),
    mongo_bytes:int32(ResponseTo),
    mongo_bytes:int32(OpCode).

parse_response_info(Info) -->
    { Info = info(Flags,CursorId,StartingFrom,NumberReturned) },
    mongo_bytes:int32(Flags),
    mongo_bytes:int64(CursorId),
    mongo_bytes:int32(StartingFrom),
    mongo_bytes:int32(NumberReturned).

parse_response_docs(Bytes, Docs) :-
    bson:docs_bytes(Docs, Bytes).

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
