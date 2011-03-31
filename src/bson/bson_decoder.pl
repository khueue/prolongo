/** <module> BSON Decoder
 *
 * This module processes structured comments and generates both formal
 * mode declarations from them as well as documentation in the form of
 * HTML or LaTeX.
 *
 * @author khueue
 * @license GPL
 */

:- module(_, []).

:- use_module(bson_bits).

:- begin_tests(bson_decoder).

test('hello world') :-
    Bson =
    [
        0x16,0x00,0x00,0x00,0x02,
        104, 101, 108, 108, 111,
        0x00,0x06,0x00,0x00,0x00,
        119, 111, 114, 108, 100,
        0x00,0x00
    ],
    bson_decoder:decode(Bson, Term),
    Term = bson([
        hello: world
    ]).

test('hello 32') :-
    Bson =
    [
        0xFF,0x00,0x00,0x00,
        0x10, 104, 101, 108, 108, 111, 0x00,
        0x20,0x00,0x00,0x00,
        0x00
    ],
    bson_decoder:decode(Bson, Term),
    Term = bson([
        hello: 32
    ]).

test('complex') :-
    Bson =
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
    bson_decoder:decode(Bson, Term),
    Term = bson([
        'BSON': [awesome,5.05,1986]
    ]).

test('key value pair uses colon') :-
    key_value_pair(key, value, key:value).

:- end_tests(bson_decoder).

decode(Bson, Term) :-
    phrase(decode(Term), Bson).

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

element_utf8_string(Pair) -->
    e_name(Ename),
    string(String),
    { key_value_pair(Ename, String, Pair) }.

element_int32(Pair) -->
    e_name(Ename),
    int32(Integer),
    { key_value_pair(Ename, Integer, Pair) }.

key_value_pair(Key, Value, Key:Value).

% XXX: Handle unicode (do not use cstring).
string(String) -->
    length(_Integer),
    cstring(CharList),
    { atom_codes(String, CharList) }.

length(Length) -->
    int32(Length).

int32(Integer) -->
    [Byte0,Byte1,Byte2,Byte3],
    { bson_bits:bytes_to_integer(Byte0, Byte1, Byte2, Byte3, Integer) }.

e_name(Ename) -->
    cstring(CharList),
    { atom_codes(Ename, CharList) }.

cstring([]) --> [0x00], !.
cstring([Char|Cs]) -->
    [Char],
    cstring(Cs).

end --> [0x00].
