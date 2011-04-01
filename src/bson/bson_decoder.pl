/**
 * BSON decoder.
 */

:- module(_, [decode/2]).

:- use_module(bson_bits).

:- encoding(utf8).

:- begin_tests(bson_decoder).

test('hello: world', [true(Got == Expected)]) :-
    Bson =
    [
        0x16,0x00,0x00,0x00,0x02,
        104, 101, 108, 108, 111,
        0x00,0x06,0x00,0x00,0x00,
        119, 111, 114, 108, 100,
        0x00,0x00
    ],
    Expected =
    [
        'hello': 'world'
    ],
    bson_decoder:decode(Bson, Got).

test('utf8', [true(Got == Expected)]) :-
    Bson =
    [
        0xFF,0,0,0, % Doc length.
        0x02, % String tag.
            0xc3,0xa4, 0, % Ename, "ä\0".
            0x03,0,0,0, % String's byte length, incl. nul.
            0xc3,0xa4, 0, % String data, "ä\0".
        0 % Doc end.
    ],
    Expected =
    [
        'ä': 'ä'
    ],
    bson_decoder:decode(Bson, Got).

test('hello: 32', [true(Got == Expected)]) :-
    Bson =
    [
        0xFF,0x00,0x00,0x00,
        0x10, 104, 101, 108, 108, 111, 0x00,
        0x20,0x00,0x00,0x00,
        0x00
    ],
    Expected =
    [
        'hello': 32
    ],
    bson_decoder:decode(Bson, Got).

test('hello: 32, 64-bit', [true(Got == Expected)]) :-
    Bson =
    [
        0xFF,0x00,0x00,0x00,
        0x12,
        104, 101, 108, 108, 111, 0x00,
        0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
        0x00
    ],
    Expected =
    [
        'hello': 32
    ],
    bson_decoder:decode(Bson, Got).

test('hello: 5.05', [true(Got == Expected)]) :-
    Bson =
    [
        0xFF,0x00,0x00,0x00, % Top doc length.
        0x01, % Double tag.
        104, 101, 108, 108, 111, 0x00, % Ename "hello\0".
        51,51,51,51,51,51,20,64, % Double data 5.05.
        0x00
    ],
    Expected =
    [
        'hello': 5.05
    ],
    bson_decoder:decode(Bson, Got).

test('complex', [true(Got == Expected)]) :-
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
    Expected =
    [
        'BSON':
            [
                '0': 'awesome',
                '1': 5.05,
                '2': 1986
            ]
    ],
    bson_decoder:decode(Bson, Got).

test('invalid bson, missing terminating nul', [throws(bson_error(_))]) :-
    Bson =
    [
        0xFF,0x00,0x00,0x00,
        0x10, 104, 101, 108, 108, 111, 0x00,
        0x20,0x00,0x00,0x00
        % Missing nul at end-of-doc.
    ],
    bson_decoder:decode(Bson, _Got).

:- end_tests(bson_decoder).

decode(Bson, Term) :-
    phrase(decode(Term), Bson),
    !.
decode(_Bson, _Term) :-
    throw(bson_error('Invalid BSON')).

decode(Term) -->
    document(Term).

document(Elements) -->
    length(_Length), % XXX Ignored?
    element_list(Elements),
    end.

element_list([Element|Elements]) -->
    element(Element),
    !,
    element_list(Elements).
element_list([]) --> [].

element(Element) -->
    [0x01],
    !,
    element_double(Element).
element(Element) -->
    [0x02],
    !,
    element_utf8_string(Element).
element(Element) -->
    [0x04],
    !,
    element_document(Element).
element(Element) -->
    [0x10],
    !,
    element_int32(Element).
element(Element) -->
    [0x12],
    !,
    element_int64(Element).

element_document(Pair) -->
    key_name(Ename),
    document(Doc),
    { key_value_pair(Ename, Doc, Pair) }.

element_double(Pair) -->
    key_name(Ename),
    value_double(Double),
    { key_value_pair(Ename, Double, Pair) }.

element_utf8_string(Pair) -->
    key_name(Ename),
    value_string(String),
    { key_value_pair(Ename, String, Pair) }.

element_int32(Pair) -->
    key_name(Ename),
    value_int32(Integer),
    { key_value_pair(Ename, Integer, Pair) }.

element_int64(Pair) -->
    key_name(Ename),
    value_int64(Integer),
    { key_value_pair(Ename, Integer, Pair) }.

key_value_pair(Key, Value, Key:Value).

value_string(String) -->
    length(Length),
    value_utf8_string(ByteList, Length),
    { bytes_to_code_points(ByteList, String) }.

% A bit of a hack, but in order to interpret raw bytes as UTF-8
% we use a memory file as a temporary buffer, fill it with the
% bytes and then read them back, interpreting them as UTF-8.
% See: <http://www.swi-prolog.org/pldoc/doc_for?object=memory_file_to_atom/3>
bytes_to_code_points(Bytes, Utf8Atom) :-
    new_memory_file(MemFile),
    write_bytes_to_memory_file(Bytes, MemFile),
    memory_file_to_codes(MemFile, CodePoints, utf8),
    free_memory_file(MemFile),
    atom_codes(Utf8Atom, CodePoints).

write_bytes_to_memory_file(Bytes, MemFile) :-
    open_memory_file(MemFile, write, Stream, [encoding(octet)]),
    put_bytes(Bytes, Stream),
    close(Stream).

put_bytes([], _Stream).
put_bytes([Byte|Bs], Stream) :-
    put_byte(Stream, Byte),
    put_bytes(Bs, Stream).

/*
% Old (shorter) version using more atom construction. XXX Benchmark.
bytes_to_code_points(Bytes, Utf8Atom) :-
    atom_chars(RawAtom, Bytes),
    atom_to_memory_file(RawAtom, MemFile),
    memory_file_to_codes(MemFile, CodePoints, utf8),
    free_memory_file(MemFile),
    atom_codes(Utf8Atom, CodePoints).
*/

value_utf8_string(CharList, Length) -->
    { LengthMinusNul is Length - 1 },
    value_utf8_string(CharList, 0, LengthMinusNul).

value_utf8_string([], Length, Length) --> [0x00], !.
value_utf8_string([Byte|Bs], Length0, Length) -->
    [Byte],
    { Length1 is Length0 + 1 },
    value_utf8_string(Bs, Length1, Length).

value_double(Double) -->
    double(Double).

value_int32(Integer) -->
    int32(Integer).

value_int64(Integer) -->
    int64(Integer).

double(Double) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_float(B0, B1, B2, B3, B4, B5, B6, B7, Double) }.

int32(Integer) -->
    [B0,B1,B2,B3],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, Integer) }.

int64(Integer) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, B4, B5, B6, B7, Integer) }.

key_name(Ename) -->
    cstring(CharList),
    { bytes_to_code_points(CharList, Ename) }.

length(Length) -->
    int32(Length).

cstring([]) --> [0x00], !.
cstring([Char|Cs]) -->
    [Char],
    cstring(Cs).

end --> [0x00].
