:- module(bson_encoder,
    [
        doc_to_bytes/2,
        doc_to_bytes/3,
        docs_to_bytes/2,
        docs_to_bytes/3
    ]).

/** <module> BSON encoder.
 *
 *  This module is private. See bson.
 */

:- include(misc(common)).

:- use_module(bson_bits, []).
:- use_module(bson_unicode, []).

%%  doc_to_bytes(+Doc, ?Bytes) is semidet.
%%  doc_to_bytes(+Doc, ?Bytes, ?NumBytes) is semidet.
%
%   True if Bytes is the flat-list BSON byte-encoding of Doc.
%   NumBytes is the number of bytes in Bytes.
%
%   @throws bson_error(Reason)

doc_to_bytes(Doc, Bytes) :-
    doc_to_bytes(Doc, Bytes, _NumBytes).

doc_to_bytes(Doc, Bytes, NumBytes) :-
    docs_to_bytes([Doc], Bytes, NumBytes).

%%  docs_to_bytes(+Docs, ?Bytes) is semidet.
%%  docs_to_bytes(+Docs, ?Bytes, ?NumBytes) is semidet.
%
%   True if Bytes is the flat-list BSON byte-encoding of all the
%   documents in the list Docs.
%   NumBytes is the total number of bytes in Bytes.
%
%   @throws bson_error(Reason)

docs_to_bytes(Docs, Bytes) :-
    docs_to_bytes(Docs, Bytes, _NumBytes).

docs_to_bytes(Docs, Bytes, NumBytes) :-
    docs_to_bytes(Docs, Bytes, 0, NumBytes).

docs_to_bytes([], [], Len, Len) :- !.
docs_to_bytes([Doc|Docs], Bytes, Len0, Len) :-
    phrase(document(Doc, Len1), Bytes, RestBytes),
    !,
    Len2 is Len0 + Len1,
    docs_to_bytes(Docs, RestBytes, Len2, Len).
docs_to_bytes(_Docs, _, _, _) :-
    throw(bson_error(invalid)).

document(Elements, Len) -->
    bytes_n(BytesForLen, 4),
    elements(Elements, LenElements),
    [0],
    { Len is 4 + LenElements + 1 },
    { bson_bits:integer_bytes(Len, 4, little, BytesForLen) }.

elements(Elements, Len) -->
    elements(Elements, 0, Len).

elements([], Len, Len) --> [].
elements([Key-Value|Elements], Len0, Len) -->
    element(Key, Value, LenElement),
    { Len1 is Len0 + LenElement },
    elements(Elements, Len1, Len).

element(Key, Value, Len) -->
    [Tag],
    key(Key, KeyLen),
    value(Value, Tag, ValueLen),
    { Len is 1 + KeyLen + ValueLen }.

key(Key, Len) -->
    c_string(Key, Len).

value(Value, Tag, Len) -->
    { core:integer(Value) }, !,
    value_integer(Value, Tag, Len).
value(Value, Tag, Len) -->
    { core:float(Value) }, !,
    value_float(Value, Tag, Len).
value(Value, Tag, Len) -->
    { util:list_shaped(Value) }, !, % Must be before atom, [] is considered one!
    value_list(Value, Tag, Len).
value(Value, Tag, Len) -->
    { core:atom(Value) }, !,
    value_atom(Value, Tag, Len).
value(Value, Tag, Len) -->
    { core:compound(Value) }, !,
    value_compound(Value, Tag, Len).

value_compound(+Constant, Tag, Len) -->
    value_constant(Constant, Tag, Len).
value_compound(binary(Subtype,Bytes), 0x05, Len) -->
    { lists:length(Bytes, BytesLen) },
    int32(BytesLen),
    subtype(Subtype),
    Bytes,
    { Len is 4 + 1 + BytesLen }.
value_compound(object_id(ObjectId), 0x07, 12) -->
    { bson_bits:hex_bytes(ObjectId, ObjectIdBytes) },
    ObjectIdBytes.
value_compound(utc(Timestamp), 0x09, 8) -->
    int64(Timestamp).
value_compound(regex(Pattern,Options), 0x0b, Len) -->
    c_string(Pattern, PatternLen),
    c_string(Options, OptionsLen),
    { Len is PatternLen + OptionsLen }.
value_compound(db_pointer(Text,ObjectId), 0x0c, Len) --> % Deprecated.
    string(Text, StrLen),
    { bson_bits:hex_bytes(ObjectId, ObjectIdBytes) },
    ObjectIdBytes,
    { Len is StrLen + 12 }.
value_compound(js(JsText), 0x0d, Len) -->
    string(JsText, Len).
value_compound(js(JsText,MappingsDoc), 0x0f, Len) -->
    bytes_n(BytesForLen, 4),
    string(JsText, StrLen),
    document(MappingsDoc, DocLen),
    { Len is 4 + StrLen + DocLen },
    { bson_bits:integer_bytes(Len, 4, little, BytesForLen) }.
value_compound(mongostamp(Timestamp), 0x11, 8) -->
    int64(Timestamp).
value_compound(symbol(Atom), 0x0e, Len) -->
    string(Atom, Len).
value_compound(Compound, _Tag, _Len) -->
    { throw(bson_error(invalid(Compound))) }.

subtype(generic)      --> !, [0x00].
subtype(function)     --> !, [0x01].
subtype(old_generic)  --> !, [0x02].
subtype(uuid_old)     --> !, [0x03].
subtype(uuid)         --> !, [0x04].
subtype(md5)          --> !, [0x05].
subtype(user_defined) --> !, [0x80].
subtype(Subtype)      -->
    { throw(bson_error(unknown_subtype(Subtype))) }.

value_list(Pairs, 0x03, Len) -->
    document(Pairs, Len),
    !.
value_list(List, 0x04, Len) -->
    { add_array_keys(List, Pairs) },
    document(Pairs, Len),
    !.

add_array_keys(List, Array) :-
    add_array_keys(List, 0, Array).

add_array_keys([], _Index, []).
add_array_keys([Value|Values], Index, [Key-Value|Pairs]) :-
    core:atom_number(Key, Index),
    Index1 is Index + 1,
    add_array_keys(Values, Index1, Pairs).

value_float(Float, 0x01, 8) -->
    { bson_bits:float_bytes(Float, Bytes) },
    Bytes.

value_integer(Integer, 0x10, 4) -->
    { bson_bits:fits_in_32_bits(Integer) },
    !,
    int32(Integer).
value_integer(Integer, 0x12, 8) -->
    { bson_bits:fits_in_64_bits(Integer) },
    !,
    int64(Integer).
value_integer(Integer, _, _) -->
    { throw(bson_error(too_large(Integer))) }.

bytes_n([], 0) --> [], !.
bytes_n([Byte|Bytes], Len0) -->
    [Byte], % May be anything.
    { Len1 is Len0 - 1 },
    bytes_n(Bytes, Len1).

int32(Integer) -->
    int_size(Integer, 4).

int64(Integer) -->
    int_size(Integer, 8).

int_size(Integer, N) -->
    { bson_bits:integer_bytes(Integer, N, little, Bytes) },
    Bytes.

value_constant(undefined, 0x06, 0) --> [], !. % Deprecated.
value_constant(false,     0x08, 1) --> [0], !.
value_constant(true,      0x08, 1) --> [1], !.
value_constant(null,      0x0a, 0) --> [], !.
value_constant(min,       0xff, 0) --> [], !.
value_constant(max,       0x7f, 0) --> [], !.
value_constant(Constant,  _,    _) -->
    { throw(bson_error(unknown_constant(Constant))) }.

value_atom(Atom, 0x02, Len) -->
    string(Atom, Len).

c_string(Utf8, Len) -->
    { bson_unicode:utf8_bytes_size(Utf8, Bytes, NumBytes) },
    { Len is NumBytes + 1 },
    Bytes,
    [0].

string(Utf8, Len) -->
    { bson_unicode:utf8_bytes_size(Utf8, Bytes, NumBytes) },
    { NumBytesWithNul is NumBytes + 1 },
    { Len is 4 + NumBytesWithNul },
    int32(NumBytesWithNul),
    Bytes,
    [0].
