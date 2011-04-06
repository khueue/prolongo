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
    phrase(document(Term), Bson),
    !.
term_to_bson(_Term, _Bson) :-
    throw(bson_error(invalid)).

document(Elements) -->
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

value(Value, 0x02, Len) -->
    { builtin:atom(Value) },
    !,
    value_string(Value, Len).

value_string(Text, Len) -->
    { bson_unicode:utf8_bytes(Text, Bytes) },
    { lists:length(Bytes, StrLen) },
    { StrLenNul is StrLen + 1 },
    { Len is 4 + StrLenNul },
    { int32_to_bytes(StrLenNul, L0, L1, L2, L3) },
    [L0,L1,L2,L3],
    Bytes,
    [0].

int32_to_bytes(Int32, L0, L1, L2, L3) :-
    L0 is (Int32 >> (0*8)) /\ 0xFF,
    L1 is (Int32 >> (1*8)) /\ 0xFF,
    L2 is (Int32 >> (2*8)) /\ 0xFF,
    L3 is (Int32 >> (3*8)) /\ 0xFF.

int64_to_bytes(Int64, L0, L1, L2, L3, L4, L5, L6, L7) :-
    L0 is (Int64 >> (0*8)) /\ 0xFF,
    L1 is (Int64 >> (1*8)) /\ 0xFF,
    L2 is (Int64 >> (2*8)) /\ 0xFF,
    L3 is (Int64 >> (3*8)) /\ 0xFF,
    L4 is (Int64 >> (4*8)) /\ 0xFF,
    L5 is (Int64 >> (5*8)) /\ 0xFF,
    L6 is (Int64 >> (6*8)) /\ 0xFF,
    L7 is (Int64 >> (7*8)) /\ 0xFF.
