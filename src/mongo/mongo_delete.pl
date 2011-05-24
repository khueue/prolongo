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

delete(Coll, Selector) :-
    mongo_collection:get_namespace(Coll, FullCollName),
    bytes_delete_message(),
    mongo_collection:get_connection(Coll, Conn),
    mongo_connection:send_to_server(Conn, BytesSend).

bytes_delete_message(FullCollName, Selector)
    phrase(build_delete_bytes(FullCollName, Selector), BytesSend),
    count_bytes_and_set_length(BytesSend),

bytes_delete_message(FullCollName, Selector) -->
    build_header(45678, 45678, 2006),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(FullCollName),
    mongo_bytes:int32(0), % Flags.
    build_bson_doc(Selector).
