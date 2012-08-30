/** <module> Cursor handling.
 */

:- module(_,
    [
        cursor_kill/1,
        cursor_kill_batch/1,
        cursor_get_more/4,
        cursor_has_more/1,
        cursor_exhaust/2
    ]).

:- include(misc(common)).

new_cursor(Collection, CursorId, Cursor) :-
    Cursor = cursor(Collection, CursorId).

cursor_collection(Cursor, Collection) :-
    util:get_arg(Cursor, 1, Collection).

cursor_id(Cursor, CursorId) :-
    util:get_arg(Cursor, 2, CursorId).

%%  cursor_kill(+Cursor) is det.
%
%   Tells the database to destroy Cursor, rendering it invalid.
%   Note that executing cursor_has_more/1 on the cursor after killing it
%   will still yield true if true before. Simply do not use after killing.

cursor_kill(Cursor) :-
    cursor_kill_batch([Cursor]).

%%  cursor_kill_batch(+Cursors) is det.
%
%   Tells the database to destroy all cursors in list Cursors, rendering
%   them invalid. See kill/1.
%
%   Caveat: Assumes that all cursors use the same database connection.

cursor_kill_batch([]) :- !.
cursor_kill_batch(Cursors) :-
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
    % XXX mongo_bytes:header(RequestId, ResponseId, OpCode)
    mongo_bytes:header(000, 000, 2007),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:int32(NumCursors),
    mongo_bytes:int64s(CursorIds).

%%  cursor_get_more(+Cursor, +Limit, -Docs, -NewCursor) is det.
%
%   True if Docs is the next batch of Limit number of documents returned
%   by Cursor, and NewCursor is the updated cursor used for further
%   fetching. Cursor is rendered unusable and must not be used after
%   this call.

cursor_get_more(Cursor, Limit, Docs, NewCursor) :-
    cursor_collection(Cursor, Collection),
    cursor_id(Cursor, CursorId),
    mongo_collection:collection_namespace(Collection, Namespace),
    build_bytes_for_cursor_get_more(Namespace, Limit, CursorId, BytesToSend),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesToSend),
    mongo_connection:read_reply(Connection, _Header, Info, Docs),
    Info = info(_Flags,CursorId1,_StartingFrom,_NumberReturned),
    new_cursor(Collection, CursorId1, NewCursor).

build_bytes_for_cursor_get_more(Namespace, Limit, CursorId, Bytes) :-
    phrase(build_bytes_for_cursor_get_more(Namespace, Limit, CursorId), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_cursor_get_more(Namespace, Limit, CursorId) -->
    % XXX mongo_bytes:header(RequestId, ResponseId, OpCode)
    mongo_bytes:header(000, 000, 2005),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(Limit),
    mongo_bytes:int64(CursorId).

%%  cursor_has_more(+Cursor) is det.
%
%   True if Cursor might have unfetched documents.

cursor_has_more(Cursor) :-
    cursor_id(Cursor, CursorId),
    CursorId \== 0.

%%  cursor_exhaust(+Cursor, -Docs) is det.
%
%   True if Docs is all the unfetched documents pointed to by Cursor before
%   this call. Cursor gets automatically killed and must not be used
%   afterwards.

cursor_exhaust(Cursor, Docs) :-
    phrase(cursor_exhaust(Cursor), Docs).

cursor_exhaust(Cursor) -->
    { cursor_has_more(Cursor) },
    !,
    % XXX Fetching default number of docs -- good/bad? Fetch more?
    { cursor_get_more(Cursor, 0, Docs, Cursor1) },
    Docs,
    cursor_exhaust(Cursor1).
cursor_exhaust(_Cursor) -->
    [].
