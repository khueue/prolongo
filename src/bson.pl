%%% Implements BSON 1.0.

:- module(bson, [encode/2,decode/2]).

:- include(includes).

test_decode :-
    % \x16\x00\x00\x00\x02hello\x00\x06\x00\x00\x00world\x00\x00
    _BsonHelloWorld = [
        0x16,0x00,0x00,0x00,0x02,
        104, 101, 108, 108, 111,
        0x00,0x06,0x00,0x00,0x00,
        119, 111, 114, 108, 100,
        0x00,0x00],
    BsonHello32 = [
        0xFF,0x00,0x00,0x00,
        0x10, 104, 101, 108, 108, 111, 0x00,
        0x20,0x00,0x00,0x00,
        0x00
    ],
    Bson = BsonHello32,
    bson:decode(Bson, Term),
    io:format('BSON: ~w~n', [Bson]),
    io:format('Term: ~w~n', [Term]).

encode(Term, Bson) :-
    % XXX Stub.
    Term = Bson.

decode(Bson, Term) :-
    decode(Term, Bson, []).

decode(Term) -->
    document(Term).

document(bson(Elements)) -->
    length(_Length),
    element_list(Elements),
    end.

element_list([Element|Elements]) -->
    element(Element), !,
    element_list(Elements).
element_list([]) --> [].

element(Element) -->
    [0x02], !,
    element_utf8_string(Element).
element(Element) -->
    [0x10], !,
    element_int32(Element).

element_utf8_string((Ename,String)) -->
    e_name(Ename),
    string(String).

element_int32((Ename,Integer)) -->
    e_name(Ename),
    int32(Integer).

% XXX: Handle unicode (do not use cstring).
string(String) -->
    length(_Integer),
    cstring(AsciiList),
    { atom_asciilist(String, AsciiList) }.

length(Length) -->
    int32(Length).

int32(Integer) -->
    [Byte0,Byte1,Byte2,Byte3],
    { bytes_to_integer(Byte0, Byte1, Byte2, Byte3, Integer) }.

e_name(Ename) -->
    cstring(AsciiList),
    { atom_asciilist(Ename, AsciiList) }.

cstring([]) --> [0x00], !.
cstring([Char|Cs]) -->
    [Char],
    cstring(Cs).

end --> [0x00].

atom_asciilist(Atom, AsciiList) :-
    name(Atom, AsciiList).

bytes_to_integer(Byte0, Byte1, Byte2, Byte3, Integer) :-
    Integer is
        (Byte0 << (8*0)) +
        (Byte1 << (8*1)) +
        (Byte2 << (8*2)) +
        (Byte3 << (8*3)).

bytes_to_integer(
    Byte0, Byte1, Byte2, Byte3,
    Byte4, Byte5, Byte6, Byte7, Integer) :-
    Integer is
        (Byte0 << (8*0)) +
        (Byte1 << (8*1)) +
        (Byte2 << (8*2)) +
        (Byte3 << (8*3)) +
        (Byte4 << (8*4)) +
        (Byte5 << (8*5)) +
        (Byte6 << (8*6)) +
        (Byte7 << (8*7)).
