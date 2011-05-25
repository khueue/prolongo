:- module(mongo_update,
    [
        upsert/3,
        update/3,
        update_all/3
    ]).

/** <module> xxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_bytes), []).
:- use_module(mongo(mongo_collection), []).
:- use_module(mongo(mongo_connection), []).
:- use_module(mongo(mongo_database), []).
:- use_module(mongo(mongo_util), []).

%%  upsert.
%
%   xxxxxxxxxx

upsert(Collection, Selector, Modifier) :-
    mongo_collection:get_namespace(Collection, Namespace),
    options_flags([upsert], Flags),
    build_message_bytes(Namespace, Selector, Modifier, Flags, BytesSend),
    mongo_collection:get_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesSend).

%%  update_all.
%
%   xxxxxxxxxx

update_all(Collection, Selector, Modifier) :-
    mongo_collection:get_namespace(Collection, Namespace),
    options_flags([multi], Flags),
    build_message_bytes(Namespace, Selector, Modifier, Flags, BytesSend),
    mongo_collection:get_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesSend).

%%  update.
%
%   xxxxxxxxxx

update(Collection, Selector, Modifier) :-
    mongo_collection:get_namespace(Collection, Namespace),
    options_flags([], Flags),
    build_message_bytes(Namespace, Selector, Modifier, Flags, BytesSend),
    mongo_collection:get_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesSend).

build_message_bytes(Namespace, Selector, Modifier, Flags, Bytes) :-
    phrase(build_message_bytes(Namespace, Selector, Modifier, Flags), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_message_bytes(Namespace, Selector, Modifier, Flags) -->
    mongo_bytes:build_header(765, 765, 2001),
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(Flags),
    mongo_bytes:build_bson_doc(Selector),
    mongo_bytes:build_bson_doc(Modifier).

options_flags([upsert], 1) :- !.
options_flags([multi],  2) :- !.
options_flags([],       0) :- !.