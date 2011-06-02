:- module(mongo_delete,
    [
        delete/2,
        delete/3
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
    delete(Collection, Selector, []).

option_value(single_remove, 1).

%%  delete.
%
%   xxxxxxxxxx

delete(Collection, Selector, Options) :-
    mongo_collection:collection_namespace(Collection, Namespace),
    mongo_util:options_flags(Options, mongo_delete:option_value, Flags),
    build_bytes_for_delete(Namespace, Selector, Flags, BytesToSend),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesToSend).

build_bytes_for_delete(Namespace, Selector, Flags, Bytes) :-
    phrase(build_bytes_for_delete(Namespace, Selector, Flags), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_delete(Namespace, Selector, Flags) -->
    mongo_bytes:header(000, 000, 2006), % xxxxx request, response
    mongo_bytes:int32(0), % ZERO.
    mongo_bytes:c_string(Namespace),
    mongo_bytes:int32(Flags),
    mongo_bytes:bson_doc(Selector).
