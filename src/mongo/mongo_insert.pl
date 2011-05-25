:- module(mongo_insert,
    [
        insert/2,
        insert_batch/3
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_bytes), []).
:- use_module(mongo(mongo_collection), []).
:- use_module(mongo(mongo_connection), []).
:- use_module(mongo(mongo_database), []).
:- use_module(mongo(mongo_util), []).

insert(Collection, Doc) :-
    insert_batch(Collection, [], [Doc]).

insert_batch(Collection, Options, Docs) :-
    mongo_collection:collection_namespace(Collection, Namespace),
    insert_batch_options_flags(Options, Flags),
    build_insert_batch_bytes(Namespace, Flags, Docs, BytesSend),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesSend).

insert_batch_options_flags([keep_going], 1) :- !.
insert_batch_options_flags([],           0) :- !.

build_insert_batch_bytes(Namespace, Flags, Docs, Bytes) :-
    phrase(build_insert_batch_bytes(Namespace, Flags, Docs), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_insert_batch_bytes(Namespace, Flags, Docs) -->
    mongo_bytes:build_header(45678, 45678, 2002), % xxxxxxx
    mongo_bytes:int32(Flags),
    mongo_bytes:c_string(Namespace),
    mongo_bytes:build_bson_docs(Docs).
