/** <module> Document insertion.
 */

:- module(mongo_insert,
    [
        insert/2,
        insert_batch/3
    ]).

:- include(misc(common)).

%%  insert(+Collection, +Doc) is det.
%
%   True if Doc is inserted into Collection.

insert(Collection, Doc) :-
    insert_batch(Collection, [], [Doc]).

%%  insert_batch(+Collection, +Options, +Docs) is det.
%
%   True if Docs are inserted into Collection using Options, where
%   possible options are:
%       keep_going

insert_batch(Collection, Options, Docs) :-
    mongo_collection:collection_namespace(Collection, Namespace),
    mongo_util:options_to_bitmask(Options, mongo_find:option_bitmask, Flags),
    build_bytes_for_insert_batch(Namespace, Flags, Docs, BytesToSend),
    mongo_collection:collection_connection(Collection, Connection),
    mongo_connection:send_to_server(Connection, BytesToSend).

build_bytes_for_insert_batch(Namespace, Flags, Docs, Bytes) :-
    phrase(build_bytes_for_insert_batch(Namespace, Flags, Docs), Bytes),
    mongo_bytes:count_bytes_and_set_length(Bytes).

build_bytes_for_insert_batch(Namespace, Flags, Docs) -->
    mongo_bytes:header(000, 000, 2002), % xxxxx request, response
    mongo_bytes:int32(Flags),
    mongo_bytes:c_string(Namespace),
    mongo_bytes:bson_docs(Docs).

%   option_bitmask(+Option, ?Bitmask) is semidet.
%
%   True if Bitmask is the bitmask for Option.

option_bitmask(keep_going, 1).
