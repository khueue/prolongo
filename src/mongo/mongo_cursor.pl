:- module(mongo_cursor,
    [
        kill/1,
        kill_batch/1,
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

new_cursor(Collection, CursorId, Cursor) :-
    Cursor = cursor(Collection, CursorId).

cursor_collection(Cursor, Collection) :-
    util:get_arg(Cursor, 1, Collection).

cursor_id(Cursor, CursorId) :-
    util:get_arg(Cursor, 2, CursorId).

%%  kill(+Cursor) is det.
%
%   Tells the database to destroy Cursor, rendering it invalid.
%   Note that executing has_more/1 on the cursor after killing it will still
%   yield true if true before. Simply do not use after killing.

kill(Cursor) :-
    kill_batch([Cursor]).

%%  kill_batch(+Cursors) is det.
%
%   Tells the database to destroy all cursors in list Cursors, rendering
%   them invalid. See kill/1.
%
%   Caveat: Assumes that all cursors use the same database connection.

kill_batch([]) :- !.
kill_batch(Cursors) :-
    lists:length(Cursors, NumCursors),
    extract_cursor_ids(Cursors, CursorIds),
    build_bytes_for_kill_batch(NumCursors, CursorIds, BytesToSend),
    Cursors = [Cursor|_],
    cursor_collection(Cursor, Collection),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesToSend).

extract_cursor_ids([], []).
extract_cursor_ids([Cursor|Cursors], [Id|Ids]) :-
    cursor_id(Cursor, Id),
    extract_cursor_ids(Cursors, Ids).

build_bytes_for_kill_batch(NumCursors, CursorIds, Bytes) :-
    phrase(build_bytes_for_kill_batch(NumCursors, CursorIds), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_kill_batch(NumCursors, CursorIds) -->
    mongo_bytes:header(000, 000, 2007), % xxxxx request, response
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:int32(NumCursors),
    mongo_bytes:int64s(CursorIds).

%%  get_more(+Cursor, +Limit, -Docs, -NewCursor).
%
%   xxxxxxxx

get_more(Cursor, Limit, Docs, NewCursor) :-
    cursor_collection(Cursor, Collection),
    cursor_id(Cursor, CursorId),
    mongo_collection:collection_namespace(Collection, Namespace),
    build_bytes_for_get_more(Namespace, Limit, CursorId, BytesToSend),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesToSend),
    mongo_connection:read_reply(Connection, _Header, Info, Docs),
    Info = info(_Flags,CursorId1,_StartingFrom,_NumberReturned),
    new_cursor(Collection, CursorId1, NewCursor).

build_bytes_for_get_more(Namespace, Limit, CursorId, Bytes) :-
    phrase(build_bytes_for_get_more(Namespace, Limit, CursorId), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_get_more(Namespace, Limit, CursorId) -->
    mongo_bytes:header(000, 000, 2005), % xxxxx request, response
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(Limit),
    mongo_bytes:int64(CursorId).

%%  has_more(+Cursor) is det.
%
%   True if Cursor might have unfetched documents.

has_more(Cursor) :-
    cursor_id(Cursor, CursorId),
    CursorId \== 0.

%%  exhaust(+Cursor, -Docs) is det.
%
%   True if Docs is all the unfetched documents pointed to by Cursor before
%   this call. Cursor gets automatically killed and must not be used
%   afterwards.

exhaust(Cursor, Docs) :-
    phrase(exhaust(Cursor), Docs).

exhaust(Cursor) -->
    { has_more(Cursor) },
    !,
    % XXX Fetching default number of docs -- good/bad? Fetch more?
    { get_more(Cursor, 0, Docs, Cursor1) },
    Docs,
    exhaust(Cursor1).
exhaust(_Cursor) -->
    [].
