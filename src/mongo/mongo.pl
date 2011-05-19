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

% Defaults.
mongo_default_host(localhost).
mongo_default_port(27017).

command_namespace('$cmd').

deletetest(Docs) :-
    mongo:new_mongo(Mongo0),
    mongo:use_database(Mongo0, prolongo, Mongo),
    mongo:delete(Mongo, testcoll, [key-set]),
    mongo:find(Mongo, testcoll, [key-set], [num-1], 0, 20, _Cursor, Docs),
    mongo:free_mongo(Mongo).

delete(Mongo, Collection, Selector) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Collection, FullCollName),
    phrase(build_delete_bytes(FullCollName, Selector), BytesSend),
    count_bytes_and_set_length(BytesSend),
    send_to_server(Mongo, BytesSend).

build_delete_bytes(FullCollName, Selector) -->
    build_header(_BytesLength, 45678, 45678, 2006),
    build_flags(0), % xxx zero
    build_namespace(FullCollName),
    build_flags(0), % xxx
    build_query(Selector).

find(Mongo, Collection, Query, ReturnFields,
    Skip, Limit, cursor(Mongo,Collection,CursorId), Docs)
:-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Collection, FullCollName),
    phrase(build_find_bytes(FullCollName, Query, ReturnFields, Skip, Limit), BytesFind),
    count_bytes_and_set_length(BytesFind),
    send_to_server(Mongo, BytesFind),
    read_from_server(Mongo, BytesReply),
    inspect_response_bytes(BytesReply), %%% xxx
    parse_response_bytes_real_good(
        BytesReply,
        _Header,
        _ResponseFlags, CursorId, _StartingFrom, _NumberReturned,
        Docs).

build_find_bytes(FullCollName, Query, ReturnFields, Skip, Limit) -->
    build_header(_BytesLength, 4567, 4567, 2004),
    build_flags(0), % xxx
    build_namespace(FullCollName),
    build_num_skip(Skip),
    build_num_return(Limit),
    build_query(Query),
    build_return_field_selector(ReturnFields).

cursor_kill(cursor(Mongo,_Coll,CursorId)) :-
    phrase(build_cursor_kill_bytes(CursorId), BytesSend),
    count_bytes_and_set_length(BytesSend),
    send_to_server(Mongo, BytesSend).

build_cursor_kill_bytes(CursorId) -->
    build_header(_BytesLength, 4567, 4567, 2007),
    build_flags(0), % xxx zero
    int32little(1), % num cursor ids
    int64little(CursorId).

cursor_get_more(cursor(Mongo,Coll,CursorId), Limit, Docs, cursor(Mongo,Coll,CursorId1)) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Coll, FullCollName),
    phrase(build_get_more_bytes(FullCollName, Limit, CursorId), BytesGetMore),
    count_bytes_and_set_length(BytesGetMore),
    send_to_server(Mongo, BytesGetMore),
    read_from_server(Mongo, BytesReply),
    inspect_response_bytes(BytesReply), %%% xxx
    parse_response_bytes_real_good(
        BytesReply,
        _Header,
        _ResponseFlags, CursorId1, _StartingFrom, _NumberReturned,
        Docs).

build_get_more_bytes(FullCollName, Limit, CursorId) -->
    build_header(_BytesLength, 4567, 4567, 2005),
    build_flags(0), % xxx zero
    build_namespace(FullCollName),
    build_num_return(Limit),
    int64little(CursorId).

cursor_has_more(cursor(_Mongo,_Coll,CursorId)) :-
    CursorId \== 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_one(Mongo, Collection, Query, Result) :-
    find_one(Mongo, Collection, Query, [], Result).

find_one(Mongo, Collection, Query, ReturnFields, Result) :-
    find(Mongo, Collection, Query, ReturnFields, 0, 1, _Cursor, Docs),
    reformat_result_docs(Docs, Result).

reformat_result_docs([], nil).
reformat_result_docs([Doc], Doc).

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

build_find_one_bytes(FullCollName, Query, ReturnFields) -->
    build_header(_BytesLength, 4, 4, 2004),
    build_flags(0),
    build_namespace(FullCollName),
    build_num_skip(0),
    build_num_return(1),
    build_query(Query),
    build_return_field_selector(ReturnFields).

build_header(BytesLength, RequestId, ResponseTo, OpCode) -->
    { BytesLength = [_,_,_,_] },
    BytesLength,
    build_int32little(RequestId),
    build_int32little(ResponseTo),
    build_int32little(OpCode).

build_flags(Flags) -->
    build_int32little(Flags).

build_int32little(Int) -->
    { bson_bits:integer_bytes(Int, 4, little, Bytes) },
    Bytes.

build_namespace(FullCollName) -->
    { phrase(c_string(FullCollName), BytesFullCollName) }, % xxx ???
    BytesFullCollName.

build_num_skip(Num) -->
    build_int32little(Num).

build_num_return(Num) -->
    build_int32little(Num).

build_query(Query) -->
    { bson:doc_bytes(Query, Bytes) },
    Bytes.

build_return_field_selector(ReturnFields) -->
    { bson:doc_bytes(ReturnFields, Bytes) },
    Bytes.

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

command(Mongo, Collection, Command, Result) :-
    mongo_get_database(Mongo, Database),
    full_coll_name(Database, Collection, FullCollName),
    build_command_message(FullCollName, Command, Message),
    mongo_socket_write(Mongo, Write),
    send_bytes_and_flush(Message, Write),
    mongo_socket_read(Mongo, Read),
    read_response_bytes(Read, Bytes),
    inspect_response_bytes(Bytes), %%% xxx
    phrase(parse_response_header(_), Bytes, Bytes1),
    skip_n(Bytes1, 20, Bytes2),
    bson:docs_bytes(Result, Bytes2).

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
    [  2,  0,  0,  0], % num return xxxxxxxxxxxxxxxxxxxxxxx
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
    core:get_byte(Read, Byte),
    N1 is N - 1,
    read_n_bytes(Read, N1, Bytes).

%%%%%%%%%%%% debug:

parse_response_bytes_real_good(Bytes, Header, ResponseFlags, CursorId, StartingFrom, NumberReturned, Docs)
    :-
    phrase(
        parse_response_bytes_paperwork_real_good(
            Header,
            ResponseFlags, CursorId, StartingFrom, NumberReturned),
        Bytes, Rest),
    bson:docs_bytes(Docs, Rest).

parse_response_bytes_paperwork_real_good(
    header(MessLen,RequestId,ResponseTo,OpCode),
    ResponseFlags,
    CursorId,
    StartingFrom,
    NumberReturned)
    -->
    int32little(MessLen),
    int32little(RequestId),
    int32little(ResponseTo),
    int32little(OpCode),
    int32little(ResponseFlags),
    int64little(CursorId),
    int32little(StartingFrom),
    int32little(NumberReturned).

%%%%%%%%%%%%%%%%%%

inspect_response_bytes(Bytes) :-
    core:format('~n--- Begin Response ---~n'),
    phrase(inspect_response_paperwork, Bytes, Rest),
    bson:docs_bytes(Docs, Rest),
    inspect_response_docs(Docs),
    core:format('--- End Response ---~n~n').

inspect_response_paperwork -->
    int32little(MessLen),
    { core:format('MessLen: ~p~n', [MessLen]) },
    int32little(RequestId),
    { core:format('RequestId: ~p~n', [RequestId]) },
    int32little(ResponseTo),
    { core:format('ResponseTo: ~p~n', [ResponseTo]) },
    int32little(OpCode),
    { core:format('OpCode: ~p~n', [OpCode]) },
    int32little(ResponseFlags),
    { core:format('ResponseFlags: ~p~n', [ResponseFlags]) },
    int64little(CursorId),
    { core:format('CursorId: ~p~n', [CursorId]) },
    int32little(StartingFrom),
    { core:format('StartingFrom: ~p~n', [StartingFrom]) },
    int32little(NumberReturned),
    { core:format('NumberReturned: ~p~n', [NumberReturned]) }.

inspect_response_docs([]).
inspect_response_docs([Doc|Docs]) :-
    bson_format:pp(Doc, 1, '  '), nl,
    inspect_response_docs(Docs).

int32little(Int) -->
    [L0,L1,L2,L3],
    { length4(Int, [L0,L1,L2,L3]) }.

int64little(Int) -->
    [L0,L1,L2,L3,L4,L5,L6,L7],
    { bson_bits:integer_bytes(Int, 8, little, [L0,L1,L2,L3,L4,L5,L6,L7]) }.
