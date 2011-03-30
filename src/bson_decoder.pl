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
