:- module(bson_encoder,
    [
        term_to_bson/2
    ]).

% <module> BSON encoder.

:- use_module(bson_bits, []).
:- use_module(bson_unicode, []).

:- include(misc(common)).

%%  term_to_bson(+Term, ?Bson:list) is semidet.
%
%   True if xxx.

term_to_bson(Term, Bson) :-
    phrase(document(Term, _Len), Bson),
    !.
term_to_bson(_Term, _Bson) :-
    throw(bson_error(invalid)).

document(Elements, Len) -->
    [L0,L1,L2,L3],
    elements(Elements, LenElements),
    [0],
    { Len is 4 + LenElements + 1 },
    { int32_to_bytes(Len, L0, L1, L2, L3) }.

elements(Elements, Len) -->
    elements(Elements, 0, Len).

elements([], Len, Len) --> [].
elements([Key:Value|Elements], Len0, Len) -->
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

c_string(Text, Len) -->
    { bson_unicode:utf8_bytes(Text, Bytes) },
    { lists:length(Bytes, Len0) },
    { Len is Len0 + 1 },
    Bytes,
    [0].

list_shaped([]).
list_shaped([_|_]).

value(Value, 0x01, Len) -->
    { builtin:float(Value) },
    !,
    value_double(Value, Len).
value(Value, 0x02, Len) -->
    { builtin:atom(Value) },
    !,
    value_string(Value, Len).
value(Value, 0x03, Len) -->
    { list_shaped(Value) },
    value_document(Value, Len).
value(Value, 0x04, Len) -->
    { list_shaped(Value) },
    value_array(Value, Len).
value(Value, 0x10, Len) -->
    { looks_like_int32(Value) },
    !,
    value_int32(Value, Len).
value(Value, 0x12, Len) -->
    { looks_like_int64(Value) },
    !,
    value_int64(Value, Len).

value_document(Pairs, Len) -->
    document(Pairs, Len).

value_array(List, Len) -->
    { add_array_keys(List, Pairs) },
    document(Pairs, Len).

add_array_keys(List, Array) :-
    add_array_keys(List, 0, Array).

add_array_keys([], _IndexInt, []).
add_array_keys([Value|Values], IndexInt, [Key:Value|Pairs]) :-
    builtin:atom_number(Key, IndexInt),
    IndexInt1 is IndexInt + 1,
    add_array_keys(Values, IndexInt1, Pairs).

value_double(Float, 8) -->
    { bson_bits:float_to_bytes(Float, B0, B1, B2, B3, B4, B5, B6, B7) },
    [B0,B1,B2,B3,B4,B5,B6,B7].

value_int32(Integer, 4) -->
    { int32_to_bytes(Integer, B0, B1, B2, B3) },
    [B0,B1,B2,B3].

value_int64(Integer, 8) -->
    { int64_to_bytes(Integer, B0, B1, B2, B3, B4, B5, B6, B7) },
    [B0,B1,B2,B3,B4,B5,B6,B7].

value_string(Text, Len) -->
    { bson_unicode:utf8_bytes(Text, Bytes) },
    { lists:length(Bytes, StrLen) },
    { StrLenNul is StrLen + 1 },
    { int32_to_bytes(StrLenNul, L0, L1, L2, L3) },
    { Len is 4 + StrLenNul },
    [L0,L1,L2,L3],
    Bytes,
    [0].

looks_like_int32(Value) :-
    builtin:integer(Value),
    fits_in_32_bits(Value).

looks_like_int64(Value) :-
    builtin:integer(Value),
    fits_in_64_bits(Value).

fits_in_32_bits(Int) :-
    -(2**(32-1)) =< Int, Int =< (2**(32-1))-1.

fits_in_64_bits(Int) :-
    -(2**(64-1)) =< Int, Int =< (2**(64-1))-1.

% Todo: What happens when given a too large (unbounded) integer?
int32_to_bytes(Int32, B0, B1, B2, B3) :-
    B0 is (Int32 >> (0*8)) /\ 0xFF,
    B1 is (Int32 >> (1*8)) /\ 0xFF,
    B2 is (Int32 >> (2*8)) /\ 0xFF,
    B3 is (Int32 >> (3*8)) /\ 0xFF.

% Todo: What happens when given a too large (unbounded) integer?
int64_to_bytes(Int64, B0, B1, B2, B3, B4, B5, B6, B7) :-
    B0 is (Int64 >> (0*8)) /\ 0xFF,
    B1 is (Int64 >> (1*8)) /\ 0xFF,
    B2 is (Int64 >> (2*8)) /\ 0xFF,
    B3 is (Int64 >> (3*8)) /\ 0xFF,
    B4 is (Int64 >> (4*8)) /\ 0xFF,
    B5 is (Int64 >> (5*8)) /\ 0xFF,
    B6 is (Int64 >> (6*8)) /\ 0xFF,
    B7 is (Int64 >> (7*8)) /\ 0xFF.
