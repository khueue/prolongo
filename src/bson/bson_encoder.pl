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
    atom(Text),
    !,
    atom_codes(Text, Codes),
    utf8_codes_to_bytes(Codes, Bytes).
utf8_to_bytes(Codes, Bytes) :-
    utf8_codes_to_bytes(Codes, Bytes).

utf8_codes_to_bytes(Codes, Bytes) :-
    charsio:open_chars_stream(Codes, ReadStream),
    builtin:set_stream(ReadStream, encoding(octet)),
    read_util:read_stream_to_codes(ReadStream, Bytes),
    close(ReadStream).

memory_file_to_atom_or_codes(MemFile, atom(Text), Encoding) :-
    !,
    memory_file:memory_file_to_atom(MemFile, Text, Encoding).
memory_file_to_atom_or_codes(MemFile, codes(Text), Encoding) :-
    !,
    memory_file:memory_file_to_codes(MemFile, Text, Encoding).
memory_file_to_atom_or_codes(_MemFile, Unknown, _Encoding) :-
    builtin:format(atom(Message), 'Unknown output term: ~w', [Unknown]),
    throw(internal(Message)).
