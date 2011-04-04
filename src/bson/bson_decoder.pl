% BSON decoder.

:- module(_,
    [
        decode/2
    ]).

:- use_module(bson_bits, []).

:- include(misc(common)).

%%  decode(+Bson:list, -Term) is semidet.
%
%   True if Term is the BSON document represented by the list
%   of bytes (0..255) in Bson.

:- begin_tests('bson_decoder:decode/2').

test('valid utf8', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x02, % String tag.
            0xc3,0xa4, 0, % Ename, "ä\0".
            6,0,0,0, % String's byte length, incl. nul.
            0xc3,0xa4, 0, 0xc3,0xa4, 0, % String data, "ä\0ä\0".
        0 % End of top doc.
    ],
    Expected =
    [
        'ä': 'ä\0ä'
    ],
    decode(Bson, Got).

test('nuls not allowed in ename', [throws(bson_error(_))]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x02, % String tag.
            0xc3,0xa4, 0, 0xc3,0xa4, 0, % Ename, "ä\0ä\0".
            3,0,0,0, % String's byte length, incl. nul.
            0xc3,0xa4, 0, % String data, "ä\0".
        0 % End of top doc.
    ],
    decode(Bson, _Got).

test('int32', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Int32 tag
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0, % Int32 data, 32.
        0 % End of top doc.
    ],
    Expected =
    [
        'hello': 32
    ],
    decode(Bson, Got).

test('int64', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x12, % Int64 tag
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0, 0,0,0,0, % Int64 data, 32.
        0 % End of top doc.
    ],
    Expected =
    [
        'hello': 32
    ],
    decode(Bson, Got).

test('float', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x01, % Double tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            51,51,51,51, 51,51,20,64, % Double data, 5.05.
        0 % End of top doc.
    ],
    Expected =
    [
        'hello': 5.05
    ],
    decode(Bson, Got).

test('embedded doc', [true(Got == Expected)]) :-
    Bson =
    [
        49,0,0,0, % Length of top doc.
        0x03, % Embedded doc tag.
            66,83,79,78, 0, % Ename "BSON\0".
            38,0,0,0, % Length of embedded doc (array).
            0x02, % String tag.
                48, 0, % Ename, index 0 ("0\0").
                8,0,0,0, % String's byte length, incl. nul.
                97,119,101,115,111,109,101, 0, % String data, "awesome\0".
            0x01, % Double tag.
                49, 0, % Ename, index 1 ("1\0").
                51,51,51,51,51,51,20,64, % Double 8-byte data, 5.05.
            0x10, % Int32 tag.
                50, 0, % Ename, index 2 ("2\0").
                194,7,0,0, % Int32 data, 1986.
            0, % End of embedded doc (array).
        0 % End of top doc.
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
    decode(Bson, Got).

test('embedded array', [true(Got == Expected)]) :-
    Bson =
    [
        49,0,0,0, % Length of top doc.
        0x04, % Array tag.
            66,83,79,78, 0, % Ename "BSON\0".
            38,0,0,0, % Length of embedded doc (array).
            0x02, % String tag.
                48, 0, % Ename, index 0 ("0\0").
                8,0,0,0, % String's byte length, incl. nul.
                97,119,101,115,111,109,101, 0, % String data, "awesome\0".
            0x01, % Double tag.
                49, 0, % Ename, index 1 ("1\0").
                51,51,51,51,51,51,20,64, % Double 8-byte data, 5.05.
            0x10, % Int32 tag.
                50, 0, % Ename, index 2 ("2\0").
                194,7,0,0, % Int32 data, 1986.
            0, % End of embedded doc (array).
        0 % End of top doc.
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
    decode(Bson, Got).

test('binary, generic', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Binary tag.
            66,83,79,78, 0, % Ename "BSON\0".
            5,0,0,0, % Length of binary data.
            0x00, % Subtype generic (default).
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(generic, [0,1,2,1,0])
    ],
    decode(Bson, Got).

test('binary, function', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Binary tag.
            66,83,79,78, 0, % Ename "BSON\0".
            5,0,0,0, % Length of binary data.
            0x01, % Subtype function.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(function, [0,1,2,1,0])
    ],
    decode(Bson, Got).

test('binary, old generic', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Binary tag.
            66,83,79,78, 0, % Ename "BSON\0".
            5,0,0,0, % Length of binary data.
            0x02, % Subtype old generic.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(old_generic, [0,1,2,1,0])
    ],
    decode(Bson, Got).

test('binary, uuid', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Binary tag.
            66,83,79,78, 0, % Ename "BSON\0".
            5,0,0,0, % Length of binary data.
            0x03, % Subtype UUID.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(uuid, [0,1,2,1,0])
    ],
    decode(Bson, Got).

test('binary, md5', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Binary tag.
            66,83,79,78, 0, % Ename "BSON\0".
            5,0,0,0, % Length of binary data.
            0x05, % Subtype MD5.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(md5, [0,1,2,1,0])
    ],
    decode(Bson, Got).

test('binary, user defined', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Binary tag.
            66,83,79,78, 0, % Ename "BSON\0".
            5,0,0,0, % Length of binary data.
            0x80, % Subtype user defined.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(user_defined, [0,1,2,1,0])
    ],
    decode(Bson, Got).

test('js with scope', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0F, % JS with scope tag.
            106,115, 0, % Ename "js\0".
            6,6,6,6, % int xxx
            5,0,0,0, % String's byte length, incl. nul.
            99,111,100,101, 0, % String data, "code\0".
                xxx_not_impl,0,0,0, % Length of embedded doc.
                0x10, % Int32 tag
                    104,101,108,108,111, 0, % Ename "hello\0".
                    32,0,0,0, % Int32 data, 32.
                0, % End of embedded doc.
        0 % End of top doc.
    ],
    Expected =
    [
        'js': js_with_scope('code', ['hello':32])
    ],
    decode(Bson, Got).

test('undefined', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x06, % Undefined tag.
            66,83,79,78, 0, % Ename "BSON\0".
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': undefined
    ],
    decode(Bson, Got).

test('invalid bson, missing terminating nul', [throws(bson_error(_))]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Int32 tag
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0 % Int32 data, 32.
        % Missing nul at end-of-doc.
    ],
    decode(Bson, _Got).

:- end_tests('bson_decoder:decode/2').

decode(Bson, Term) :-
    phrase(decode(Term), Bson),
    !.
decode(_Bson, _Term) :-
    throw(bson_error(invalid)).

decode(Term) -->
    document(Term).

document(Elements) -->
    length(_Length), % XXX Ignored for now. Validate how much?
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
    key_name(Ename),
    value_double(Value),
    { key_value_pair(Ename, Value, Element) }.
element(Element) -->
    [0x02],
    !,
    key_name(Name),
    value_string(Value),
    { key_value_pair(Name, Value, Element) }.
element(Element) -->
    [0x03],
    !,
    key_name(Name),
    value_document(Value),
    { key_value_pair(Name, Value, Element) }.
element(Element) -->
    [0x04],
    !,
    key_name(Name),
    value_document(Value),
    { key_value_pair(Name, Value, Element) }.
element(Element) -->
    [0x05],
    !,
    key_name(Name),
    value_binary(Value),
    { key_value_pair(Name, Value, Element) }.
element(Element) -->
    [0x06], % Deprecated in BSON 1.0.
    !,
    key_name(Name),
    value_undefined(Value),
    { key_value_pair(Name, Value, Element) }.
element(Element) -->
    [0x0F],
    !,
    key_name(Name),
    value_js_with_scope(Value),
    { key_value_pair(Name, Value, Element) }.
element(Element) -->
    [0x10],
    !,
    key_name(Name),
    value_int32(Value),
    { key_value_pair(Name, Value, Element) }.
element(Element) -->
    [0x12],
    !,
    key_name(Name),
    value_int64(Value),
    { key_value_pair(Name, Value, Element) }.

key_name(Ename) -->
    cstring(CharList),
    { bytes_to_utf8_atom(CharList, Ename) }.

key_value_pair(Key, Value, Key:Value).

value_document(Doc) -->
    document(Doc).

value_string(Atom) -->
    length(Length),
    utf8_bytes(ByteList, Length),
    { bytes_to_utf8_atom(ByteList, Atom) }.

value_binary(binary(Subtype,ByteList)) -->
    length(Length),
    subtype(Subtype),
    bytes(ByteList, Length).

value_js_with_scope(js_with_scope(Code,MappingsDoc)) -->
    length(_LengthEntireJsWithScope), % XXX Unused for now.
    value_string(Code),
    value_document(MappingsDoc).

value_undefined(undefined) -->
    [].

value_double(Double) -->
    double(Double).

value_int32(Integer) -->
    int32(Integer).

value_int64(Integer) -->
    int64(Integer).

subtype(generic)      --> [0x00], !.
subtype(function)     --> [0x01], !.
subtype(old_generic)  --> [0x02], !.
subtype(uuid)         --> [0x03], !.
subtype(md5)          --> [0x05], !.
subtype(user_defined) --> [0x80], !.

cstring([]) -->
    [0x00],
    !.
cstring([Char|Cs]) -->
    [Char], % May not be nul (caught by base case).
    cstring(Cs).

utf8_bytes(ByteList, Length) -->
    { LengthMinusNul is Length - 1 },
    bytes(ByteList, LengthMinusNul),
    [0x00].

bytes([], 0) -->
    [],
    !.
bytes([Byte|Bs], Length0) -->
    [Byte],
    { Length1 is Length0 - 1 },
    bytes(Bs, Length1).

length(Length) -->
    int32(Length).

double(Double) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_float(B0, B1, B2, B3, B4, B5, B6, B7, Double) }.

int32(Integer) -->
    [B0,B1,B2,B3],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, Integer) }.

int64(Integer) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, B4, B5, B6, B7, Integer) }.

end --> [0x00].

% A bit of a hack, but in order to interpret raw bytes as UTF-8
% we use a memory file as a temporary buffer, fill it with the
% bytes and then read them back, treating them as UTF-8.
% See: http://www.swi-prolog.org/pldoc/doc_for?object=memory_file_to_atom/3

bytes_to_utf8_atom(Bytes, Utf8Atom) :-
    builtin:atom_chars(RawAtom, Bytes),
    setup_call_cleanup(
        memory_file:atom_to_memory_file(RawAtom, MemFile),
        memory_file:memory_file_to_atom(MemFile, Utf8Atom, utf8),
        memory_file:free_memory_file(MemFile)).
