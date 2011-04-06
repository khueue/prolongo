:- module(_,
    [
        encode/2
    ]).

% <module> BSON encoder.

:- use_module(bson_bits, []).

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
    tag(Value, TagLen),
    key(Key, KeyLen),
    value(Value, ValueLen),
    { Len is TagLen + KeyLen + ValueLen }.

tag(_Atom, 1) -->
    [0x02].

key(Key, Len) -->
    { utf8_to_bytes(Key, Bytes) },
    { lists:length(Bytes, Len0) },
    { Len is Len0 + 1 },
    Bytes,
    [0].

value(Value, Len) -->
    { utf8_to_bytes(Value, Bytes) },
    { lists:length(Bytes, StrLen) },
    { StrLenNul is StrLen + 1 },
    { Len is 4 + StrLenNul },
    { int32_to_bytes(StrLenNul, L0, L1, L2, L3) },
    [L0,L1,L2,L3],
    Bytes,
    [0].

int32_to_bytes(Integer, L0, L1, L2, L3) :-
    L0 = Integer,
    L1 = 0,
    L2 = 0,
    L3 = 0.

utf8_to_bytes(Text, Bytes) :-
    builtin:atom(Text),
    !,
    builtin:atom_codes(Text, Codes),
    utf8_codes_to_bytes(Codes, Bytes).
utf8_to_bytes(Codes, Bytes) :-
    utf8_codes_to_bytes(Codes, Bytes).

utf8_codes_to_bytes(Codes, Bytes) :-
    setup_call_cleanup(
        charsio:open_chars_stream(Codes, ReadStream),
        stream_to_bytes(ReadStream, Bytes),
        builtin:close(ReadStream)).

stream_to_bytes(ReadStream, Bytes) :-
    builtin:set_stream(ReadStream, encoding(octet)),
    readutil:read_stream_to_codes(ReadStream, Bytes).
