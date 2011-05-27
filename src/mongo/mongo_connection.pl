:- module(mongo_connection,
    [
        new_connection/1,
        new_connection/3,
        free_connection/1,
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
:- use_module(mongo(mongo_socket), []).

%%  new_connection(-Connection) is semidet.
%%  new_connection(+Host, +Port, -Connection) is semidet.
%
%   xxx True if Mongo represents an opaque handle to a new MongoDB
%   server connection. Host and Port may be supplied, otherwise
%   the defaults (see mongo_defaults) are used.

new_connection(Connection) :-
    mongo_defaults:host(Host),
    mongo_defaults:port(Port),
    new_connection(Host, Port, Connection).

new_connection(Host, Port, Connection) :-
    mongo_socket:new_socket(Host, Port, Socket),
    Connection = connection(Socket).

connection_socket(Connection, Socket) :-
    util:get_arg(Connection, 1, Socket).

%%  free_connection(+Connection) is det.
%
%   xxx Frees any resources associated with the Mongo handle,
%   rendering it unusable.

free_connection(Connection) :-
    connection_socket(Connection, Socket),
    mongo_socket:free_socket(Socket).

%%  get_database(+Connection, +DatabaseName, -Database).
%
%   Database is a handle to the database DatabaseName. No communication
%   is performed so the actual database might or might not exist.

get_database(Connection, DatabaseName, Database) :-
    mongo_database:new_database(Connection, DatabaseName, Database).

%%  send_to_server.
%
%   xxxxxxxxx

send_to_server(Connection, Bytes) :-
    connection_socket(Connection, Socket),
    mongo_socket:send_bytes(Socket, Bytes).

%%  read_reply.
%
%   xxxxxxxx

read_reply(Connection, Header, Info, Docs) :-
    connection_socket(Connection, Socket),
    read_response_bytes(Socket, Bytes),
    parse_response(Bytes, Header, Info, Docs).

read_response_bytes(Socket, [B0,B1,B2,B3|Bytes]) :-
    read_message_length(Socket, [B0,B1,B2,B3], TotalLength),
    read_rest_of_message(Socket, TotalLength, Bytes).

read_message_length(Socket, Bytes, Length) :-
    mongo_socket:receive_n_bytes(Socket, 4, Bytes),
    bson_bits:integer_bytes(Length, 4, little, Bytes).

read_rest_of_message(Socket, TotalLength, Bytes) :-
    LengthRest is TotalLength - 4,
    mongo_socket:receive_n_bytes(Socket, LengthRest, Bytes).

parse_response(Bytes, Header, Info, Docs) :-
    %inspect_response_bytes(Bytes),
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

%%% XXX DEBUG:

inspect_response_bytes(Bytes) :-
    core:format('~n--- Begin Response ---~n'),
    phrase(inspect_response_paperwork, Bytes, Rest),
    bson:docs_bytes(Docs, Rest),
    inspect_response_docs(Docs),
    core:format('--- End Response ---~n~n').

inspect_response_paperwork -->
    mongo_bytes:int32(MessLen),
    { core:format('MessLen: ~p~n', [MessLen]) },
    mongo_bytes:int32(RequestId),
    { core:format('RequestId: ~p~n', [RequestId]) },
    mongo_bytes:int32(ResponseTo),
    { core:format('ResponseTo: ~p~n', [ResponseTo]) },
    mongo_bytes:int32(OpCode),
    { core:format('OpCode: ~p~n', [OpCode]) },
    mongo_bytes:int32(ResponseFlags),
    { core:format('ResponseFlags: ~p~n', [ResponseFlags]) },
    mongo_bytes:int64(CursorId),
    { core:format('CursorId: ~p~n', [CursorId]) },
    mongo_bytes:int32(StartingFrom),
    { core:format('StartingFrom: ~p~n', [StartingFrom]) },
    mongo_bytes:int32(NumberReturned),
    { core:format('NumberReturned: ~p~n', [NumberReturned]) }.

inspect_response_docs([]).
inspect_response_docs([Doc|Docs]) :-
    bson_format:pp(Doc, 1, '  '), nl,
    inspect_response_docs(Docs).
