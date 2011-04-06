:- module(bson_encoder,
    [
        encode/2
    ]).

% <module> BSON encoder.

:- use_module(bson_bits, []).
:- use_module(bson_unicode, []).

:- include(misc(common)).

%%  encode(+Term, ?Bson:list) is semidet.
%
%   True if xxx.

encode(Term, Bson) :-
    phrase(document(Term), Bson),
    !.
encode(_Term, _Bson) :-
    throw(bson_error(invalid)).

document(Elements) -->
    [L0,L1,L2,L3],
    elements(Elements, LenElements),
    [0],
    { Len is 4 + LenElements + 1 },
    { int32_to_bytes(Len, L0, L1, L2, L3) }.

elements(Elements, Len) -->
    elements(Elements, 0, Len).

elements([], Len, Len) --> !, [].
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

% XXX Implement in foreign library.
int32_to_bytes(Integer, L0, L1, L2, L3) :-
    L0 = Integer,
    L1 = 0,
    L2 = 0,
    L3 = 0.
