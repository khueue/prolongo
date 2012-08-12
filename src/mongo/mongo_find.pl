:- module(mongo_find,
    [
        find_one/3,
        find_one/4,
        find_all/4,
        find/7,
        find/8
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

%%  find_one(+Collection, +Query, -Doc).
%
%   Identical to calling find_one/4 with empty return fields (all fields).

find_one(Collection, Query, Result) :-
    find_one(Collection, Query, [], Result).

%%  find_one(+Collection, +Query, +ReturnFields, -Doc).
%
%   True if Doc is the first document in Collection that matches Query.
%   ReturnFields is a document describing which fields to return
%   (empty means all fields).

find_one(Collection, Query, ReturnFields, Doc) :-
    find(Collection, Query, ReturnFields, 0, 1, _Cursor, Docs),
    package_result_doc(Docs, Doc).

package_result_doc([], nil).
package_result_doc([Doc], Doc).

%%  find_all(+Collection, +Query, +ReturnFields, -Docs).
%
%   True if Docs is all the documents in Collection that match Query.
%   ReturnFields is a document describing which fields to return
%   (empty means all fields).

find_all(Collection, Query, ReturnFields, Docs) :-
    phrase(find_all(Collection, Query, ReturnFields), Docs).

% XXX Should be implemented using the "exhaust" flag.
find_all(Collection, Query, ReturnFields) -->
    { find(Collection, Query, ReturnFields, 0, 0, Cursor, Docs0) },
    Docs0,
    { mongo:cursor_exhaust(Cursor, DocsRest) },
    DocsRest.

option_value(tailable_cursor,     2).
option_value(slave_ok,            4).
% option_value(oplog_replay,      8). % Skip this.
option_value(no_cursor_timeout,  16).
option_value(await_data,         32).
option_value(exhaust,            64).
option_value(partial,           128).

%%  find(+Collection, +Query, +ReturnFields, +Skip, +Limit, -Cursor, -Docs).
%
%   Identical to calling find/8 without options.

find(Collection, Query, ReturnFields, Skip, Limit, Cursor, Docs) :-
    find(Collection, Query, ReturnFields, Skip, Limit, [], Cursor, Docs).

%%  find(+Collection, +Query, +ReturnFields, +Skip, +Limit, +Options, -Cursor, -Docs).
%
%   True if Docs is the first batch of documents in Collection that
%   match Query. ReturnFields is a document describing which fields to
%   return (empty means all fields). Skip is the number of documents
%   to skip, and Limit is the number of documents to return. If the
%   result contains more documents than can fit into a single response,
%   Cursor can be used to retrieve more documents. Options is a list
%   of atoms (tailable_cursor, slave_ok, no_cursor_timeout, await_data,
%   exhaust, partial).

find(Collection, Query, ReturnFields, Skip, Limit, Options, Cursor, Docs) :-
    mongo_collection:collection_namespace(Collection, Namespace),
    mongo_util:options_flags(Options, mongo_find:option_value, Flags),
    build_bytes_for_find(Namespace, Query, ReturnFields, Skip, Limit, Flags, BytesToSend),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesToSend),
    mongo_connection:read_reply(Connection, _Header, Info, Docs),
    Info = info(ReturnFlags,CursorId,_StartingFrom,_NumberReturned),
    throw_on_error(ReturnFlags, Docs),
    mongo_cursor:new_cursor(Collection, CursorId, Cursor).

throw_on_error(Flags, [ErrorDoc]) :-
    error_bit_is_set(Flags),
    throw(mongo_error(ErrorDoc)).
throw_on_error(_Flags, _Docs). % Query succeeded.

error_bit_is_set(Flags) :-
    0 < Flags /\ 2.

build_bytes_for_find(Namespace, Query, ReturnFields, Skip, Limit, Flags, Bytes) :-
    phrase(build_bytes_for_find(Namespace, Query, ReturnFields, Skip, Limit, Flags), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_find(Namespace, Query, ReturnFields, Skip, Limit, Flags) -->
    mongo_bytes:header(000, 000, 2004), % xxxxx request, response
    mongo_bytes:int32(Flags),
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(Skip),
    mongo_bytes:int32(Limit),
    mongo_bytes:bson_doc(Query),
    mongo_bytes:bson_doc(ReturnFields).
