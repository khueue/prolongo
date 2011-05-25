:- module(mongo_bytes,
    [
        int32/3,
        int64/3,
        int64s/3,
        c_string/3,
        header/5,
        bson_doc/3,
        bson_docs/3
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).

int32s([]) --> [].
int32s([Int|Ints]) -->
    int32(Int),
    int32s(Ints).

% Can parse or construct.
int32(Int) -->
    [B0,B1,B2,B3],
    { bson_bits:integer_bytes(Int, 4, little, [B0,B1,B2,B3]) }.

int64s([]) --> [].
int64s([Int|Ints]) -->
    int64(Int),
    int64s(Ints).

% Can parse or construct.
int64(Int) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:integer_bytes(Int, 8, little, [B0,B1,B2,B3,B4,B5,B6,B7]) }.

c_string(Atom) -->
    { bson_unicode:utf8_bytes(Atom, Bytes) },
    Bytes,
    [0].

header(RequestId, ResponseTo, OpCode) -->
    [_,_,_,_], % Length of entire message. Instantiate when possible.
    int32(RequestId),
    int32(ResponseTo),
    int32(OpCode).

bson_doc(Doc) -->
    { bson:doc_bytes(Doc, Bytes) },
    Bytes.

bson_docs(Docs) -->
    { bson:docs_bytes(Docs, Bytes) },
    Bytes.

count_bytes_and_set_length(Bytes) :-
    Bytes = [L0,L1,L2,L3|_],
    lists:length(Bytes, Length),
    bson_bits:integer_bytes(Length, 4, little, [L0,L1,L2,L3]).
