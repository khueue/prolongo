:- module(mongo_delete,
    [
        delete/2
    ]).

/** <module> xxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_defaults), []).
:- use_module(mongo(mongo_bytes), []).
:- use_module(mongo(mongo_connection), []).
:- use_module(mongo(mongo_collection), []).
:- use_module(mongo(mongo_database), []).
:- use_module(mongo(mongo_util), []).

%%  delete.
%
%   xxxxxxxxxx

delete(Collection, Selector) :-
    mongo_collection:get_namespace(Collection, Namespace),
    build_bytes_for_delete_message(Namespace, Selector, BytesSend),
    mongo_collection:get_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesSend).

build_bytes_for_delete_message(Namespace, Selector, Bytes) :-
    phrase(build_bytes_for_delete_message(Namespace, Selector), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_delete_message(Namespace, Selector) -->
    mongo_bytes:build_header(45678, 45678, 2006),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(0), % Flags.
    mongo_bytes:build_bson_doc(Selector).
