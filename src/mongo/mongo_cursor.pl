:- module(mongo_cursor,
    [
        kill/1,
        get_more/4,
        has_more/1,
        exhaust/2
    ]).

/** <module> xxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_bytes), []).
:- use_module(mongo(mongo_collection), []).
:- use_module(mongo(mongo_connection), []).
:- use_module(mongo(mongo_database), []).
:- use_module(mongo(mongo_util), []).

%%  kill.
%
%   xxxxxxx

kill(cursor(Collection,CursorId)) :-
    build_bytes_cursor_kill(CursorId, Bytes),
    mongo_collection:get_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, Bytes).

build_bytes_cursor_kill(CursorId, Bytes) :-
    phrase(build_bytes_cursor_kill(CursorId), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_cursor_kill(CursorId) -->
    mongo_bytes:build_header(4567, 4567, 2007),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:int32(1), % Number of cursor IDs.
    mongo_bytes:int64(CursorId). % Cursor IDs.

%%  get_more.
%
%   xxxxxxxx

get_more(cursor(Collection,CursorId), Limit, Docs, cursor(Collection,CursorId1)) :-
    mongo_collection:get_namespace(Collection, Namespace),
    build_bytes_get_more(Namespace, Limit, CursorId, Message),
    mongo_collection:get_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, Message),
    mongo_connection:read_reply(Connection, _Header, Info, Docs),
    Info = info(_Flags,CursorId1,_Start,_Num).

build_bytes_get_more(Namespace, Limit, CursorId, BytesSend) :-
    phrase(build_bytes_get_more(Namespace, Limit, CursorId), BytesSend),
    mongo_bytes:count_bytes_and_set_length(BytesSend).

build_bytes_get_more(Namespace, Limit, CursorId) -->
    mongo_bytes:build_header(4567, 4567, 2005),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(Limit),
    mongo_bytes:int64(CursorId).

%%  has_more.
%
%   xxxxxxxxxxx

has_more(cursor(_Collection,CursorId)) :-
    CursorId \== 0.

%%  exhaust.
%
%   xxxxxxxxx

exhaust(Cursor, Docs) :-
    phrase(exhaust(Cursor), Docs).

exhaust(Cursor) -->
    { has_more(Cursor) },
    !,
    % Fetching the default number of docs -- good/bad? Fetch more?
    { get_more(Cursor, 0, Docs, Cursor1) },
    Docs,
    exhaust(Cursor1).
exhaust(_Cursor) -->
    [].
