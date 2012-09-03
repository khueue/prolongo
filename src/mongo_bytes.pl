/** <module> Low-level construction of messages to send over the wire.
 */

:- module(_,
    [
        int32//1,
        int32s//1,
        int64//1,
        int64s//1,
        c_string//1,
        header//3,
        bson_doc//1,
        bson_docs//1,
        count_bytes_and_set_length/1
    ]).

:- include(misc(common)).

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
