:- module(mongo_delete,
    [
        delete/2
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

%%  delete.
%
%   xxxxxxxxxx

delete(Collection, Selector) :-
    mongo_collection:collection_namespace(Collection, Namespace),
    build_bytes_for_delete(Namespace, Selector, BytesToSend),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesToSend).

build_bytes_for_delete(Namespace, Selector, Bytes) :-
    phrase(build_bytes_for_delete(Namespace, Selector), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_delete(Namespace, Selector) -->
    mongo_bytes:header(45678, 45678, 2006),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(0), % Flags.
    mongo_bytes:bson_doc(Selector).
