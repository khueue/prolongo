%%% Implements BSON 1.0.

:- module(bson, [encode/2,decode/2]).

:- use_module(bson_bits).

test_decode :-
    % \x16\x00\x00\x00\x02hello\x00\x06\x00\x00\x00world\x00\x00
    BsonHelloWorld =
    [
        0x16,0x00,0x00,0x00,0x02,
        104, 101, 108, 108, 111,
        0x00,0x06,0x00,0x00,0x00,
        119, 111, 114, 108, 100,
        0x00,0x00
    ],
    _BsonHello32 =
    [
        0xFF,0x00,0x00,0x00,
        0x10, 104, 101, 108, 108, 111, 0x00,
        0x20,0x00,0x00,0x00,
        0x00
    ],
    _BsonAwesome =
    [
        49,0,0,0, % length
        0x04, % array tag
            66,83,79,78,0, % element name, "BSON\0"
            38,0,0,0, % length of embedded doc (array)
            0x02, % string tag
                48,0, % index 0 ("0\0")
                8,0,0,0, % length of string, incl. nul
                97,119,101,115,111,109,101, 0, % string, "awesome\0"
            0x01, % double tag
                49,0, % ename, index 1 ("1\0")
                51,51,51,51,51,51,20,64, % double 8-byte
            0x10, % int32 tag
                50,0, % ename, index 2 ("2\0")
                194,7,0,0, % int32 data (1986)
            0, % end of array doc
        0 % end of doc
    ],
    Bson = BsonHelloWorld,
    decode(Bson, Term),
    io:format('BSON: ~w~n', [Bson]),
    io:format('Term: ~w~n', [Term]),
    bson_bits:bytes_as_float(51,51,51,51, 51,51,20,64, F),
    io:format('Float: ~w~n', [F]).

putit([]).
putit([X|Xs]) :-
    put_byte(X),
    putit(Xs).

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
    element(Element),
    !,
    element_list(Elements).
element_list([]) --> [].

element(Element) -->
    [0x02],
    !,
    element_utf8_string(Element).
element(Element) -->
    [0x04],
    !,
    document(Element).
element(Element) -->
    [0x10],
    !,
    element_int32(Element).
/*
% XXX: This won't work, as it tries the doc end 0x00 and fails.
element(_Element) -->
    [Tag], !,
    { io:format('Unhandled element type: ~w~n', [Tag]), halt }.
*/

element_utf8_string((Ename,String)) -->
    e_name(Ename),
    string(String).

element_int32((Ename,Integer)) -->
    e_name(Ename),
    int32(Integer).

% XXX: Handle unicode (do not use cstring).
string(String) -->
    length(_Integer),
    cstring(CharList),
    { atom_codes(String, CharList) }.

length(Length) -->
    int32(Length).

int32(Integer) -->
    [Byte0,Byte1,Byte2,Byte3],
    { bytes_to_integer(Byte0, Byte1, Byte2, Byte3, Integer) }.

e_name(Ename) -->
    cstring(CharList),
    { atom_codes(Ename, CharList) }.

cstring([]) --> [0x00], !.
cstring([Char|Cs]) -->
    [Char],
    cstring(Cs).

end --> [0x00].

bytes_to_integer(Byte0, Byte1, Byte2, Byte3, Integer) :-
    Integer is
        (Byte0 << (8*0)) \/
        (Byte1 << (8*1)) \/
        (Byte2 << (8*2)) \/
        (Byte3 << (8*3)).

bytes_to_integer(
    Byte0, Byte1, Byte2, Byte3,
    Byte4, Byte5, Byte6, Byte7, Integer) :-
    Integer is
        (Byte0 << (8*0)) \/
        (Byte1 << (8*1)) \/
        (Byte2 << (8*2)) \/
        (Byte3 << (8*3)) \/
        (Byte4 << (8*4)) \/
        (Byte5 << (8*5)) \/
        (Byte6 << (8*6)) \/
        (Byte7 << (8*7)).
