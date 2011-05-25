:- module(mongo_find,
    [
        find_one/3,
        find_one/4,
        find_all/4,
        find/7
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

find_one(Collection, Query, Result) :-
    find_one(Collection, Query, [], Result).

find_one(Collection, Query, ReturnFields, Doc) :-
    find(Collection, Query, ReturnFields, 0, 1, _Cursor, Docs),
    package_result_doc(Docs, Doc).

package_result_doc([], nil).
package_result_doc([Doc], Doc).

find_all(Collection, Query, ReturnFields, Docs) :-
    phrase(find_all(Collection, Query, ReturnFields), Docs).

% XXX Should be implemented using the "exhaust" flag.
find_all(Collection, Query, ReturnFields) -->
    { find(Collection, Query, ReturnFields, 0, 0, Cursor, Docs0) },
    Docs0,
    { mongo:exhaust(Cursor, DocsRest) },
    DocsRest.

find(Collection, Query, ReturnFields, Skip, Limit, Cursor, Docs) :-
    Cursor = cursor(Collection,CursorId),
    mongo_collection:get_namespace(Collection, Namespace),
    build_find_bytes(Namespace, Query, ReturnFields, Skip, Limit, BytesSend),
    mongo_collection:get_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesSend),
    mongo_connection:read_reply(Connection, _Header, Info, Docs),
    Info = info(_Flags,CursorId,_StartingFrom,_NumberReturned).

build_find_bytes(Namespace, Query, ReturnFields, Skip, Limit, Bytes) :-
    phrase(build_find_bytes(Namespace, Query, ReturnFields, Skip, Limit), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_find_bytes(Namespace, Query, ReturnFields, Skip, Limit) -->
    mongo_bytes:build_header(4567, 4567, 2004),
    mongo_bytes:int32(0), % Flags. xxxxxxxx fix
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(Skip),
    mongo_bytes:int32(Limit),
    mongo_bytes:build_bson_doc(Query),
    mongo_bytes:build_bson_doc(ReturnFields).
