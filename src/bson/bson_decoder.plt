:- include(misc(common)).

:- begin_tests('bson_decoder:bson_to_term/2').

test('empty doc', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0 % End of top doc.
    ],
    Expected =
    [
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('valid utf8', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x02, % Tag.
            0xc3,0xa4, 0, % Ename.
            6,0,0,0, % String's byte length, incl. nul.
            0xc3,0xa4, 0, 0xc3,0xa4, 0, % String data.
        0 % End of top doc.
    ],
    Expected =
    [
        'ä': 'ä\0ä'
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('nuls not allowed in ename', [throws(bson_error(_))]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x02, % Tag.
            0xc3,0xa4, 0, 0xc3,0xa4, 0, % Ename.
            3,0,0,0, % String's byte length, incl. nul.
            0xc3,0xa4, 0, % String data.
        0 % End of top doc.
    ],
    bson_decoder:bson_to_term(Bson, _Got).

test('int32 positive', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            32,0,0,0, % Int32 data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: 32
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('int32 negative', [true(Got == Expected)]) :-
    Bson =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0xE0,0xFF,0xFF,0xFF, % Int32 data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: -32
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('int64 positive', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x12, % Tag.
            104,101,108,108,111, 0, % Ename.
            32,0,0,0, 0,0,0,0, % Int64 data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: 32
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('int64 negative', [true(Got == Expected)]) :-
    Bson =
    [
        20,0,0,0, % Length of top doc.
        0x12, % Tag.
            104,101,108,108,111, 0, % Ename.
            0xE0,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, % Int64 data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: -32
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('float', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x01, % Tag.
            104,101,108,108,111, 0, % Ename.
            51,51,51,51, 51,51,20,64, % Double data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: 5.05
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('embedded doc', [true(Got == Expected)]) :-
    Bson =
    [
        49,0,0,0, % Length of top doc.
        0x03, % Tag.
            66,83,79,78, 0, % Ename.
            38,0,0,0, % Length of embedded doc.
            0x02, % Tag.
                97, 0, % Ename.
                8,0,0,0, % String's byte length, incl. nul.
                97,119,101,115,111,109,101, 0, % String data.
            0x01, % Tag.
                98, 0, % Ename.
                51,51,51,51,51,51,20,64, % Double 8-byte data.
            0x10, % Tag.
                99, 0, % Ename.
                194,7,0,0, % Int32 data.
            0, % End of embedded doc.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON':
            [
                'a': 'awesome',
                'b': 5.05,
                'c': 1986
            ]
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('embedded array', [true(Got == Expected)]) :-
    Bson =
    [
        49,0,0,0, % Length of top doc.
        0x04, % Tag.
            66,83,79,78, 0, % Ename.
            38,0,0,0, % Length of embedded doc.
            0x02, % Tag.
                48, 0, % Ename, index 0.
                8,0,0,0, % String's byte length, incl. nul.
                97,119,101,115,111,109,101, 0, % String data.
            0x01, % Tag.
                49, 0, % Ename, index 1.
                51,51,51,51,51,51,20,64, % Double 8-byte data.
            0x10, % Tag.
                50, 0, % Ename, index 2.
                194,7,0,0, % Int32 data.
            0, % End of embedded doc.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON':
            [
                'awesome',
                5.05,
                1986
            ]
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('binary, generic', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Tag.
            66,83,79,78, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x00, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(generic, [0,1,2,1,0])
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('binary, function', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Tag.
            66,83,79,78, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x01, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(function, [0,1,2,1,0])
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('binary, old generic', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Tag.
            66,83,79,78, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x02, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(old_generic, [0,1,2,1,0])
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('binary, uuid', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Tag.
            66,83,79,78, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x03, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(uuid, [0,1,2,1,0])
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('binary, md5', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Tag.
            66,83,79,78, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x05, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(md5, [0,1,2,1,0])
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('binary, user defined', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x05, % Tag.
            66,83,79,78, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x80, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': binary(user_defined, [0,1,2,1,0])
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('binary, user defined', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x07, % Tag.
            66,83,79,78, 0, % Ename.
            0x47,0xcc,0x67,0x09, % ObjectID, time.
            0x34,0x75,0x06,      % ObjectID, machine.
            0x1e,0x3d,           % ObjectID, pid.
            0x95,0x36,0x9d,      % ObjectID, inc.
        0 % End of top doc.
    ],
    Expected =
    [
        'BSON': object_id('47cc67093475061e3d95369d')
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('js', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0D, % Tag.
            106,115, 0, % Ename.
            9,0,0,0, % String's byte length, incl. nul.
            99,111,100,101,32,46,46,46, 0, % String data.
        0 % End of top doc.
    ],
    Expected =
    [
        js: js('code ...')
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('js with scope', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0F, % Tag.
            106,115, 0, % Ename.
            6,6,6,6, % int XXX
            9,0,0,0, % String's byte length, incl. nul.
            99,111,100,101,32,46,46,46, 0, % String data.
                xxx_not_impl,0,0,0, % Length of embedded doc.
                0x10, % Tag.
                    104,101,108,108,111, 0, % Ename.
                    32,0,0,0, % Int32 data.
                0, % End of embedded doc.
        0 % End of top doc.
    ],
    Expected =
    [
        js: js('code ...', ['hello':32])
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('undefined', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x06, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: undefined
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('boolean true', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x08, % Tag.
            104,101,108,108,111, 0, % Ename.
            1, % Boolean data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: true
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('boolean false', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x08, % Tag.
            104,101,108,108,111, 0, % Ename.
            0, % Boolean data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: false
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('utc datetime', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x09, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,0,0,0, 0,0,0,0, % UTC datetime data. XXX better ex. data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: utc(0)
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('boolean invalid', [throws(bson_error(invalid_boolean))]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x08, % Tag.
            104,101,108,108,111, 0, % Ename.
            2, % Boolean data, INVALID value.
        0 % End of top doc.
    ],
    bson_decoder:bson_to_term(Bson, _Got).

test('null', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0A, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: null
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('regex', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0B, % Tag.
            104,101,108,108,111, 0, % Ename.
            97, 0,  % Regex pattern.
            105, 0, % Regex options.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: regex('a','i')
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('db pointer', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0C, % Tag.
            104,101,108,108,111, 0, % Ename.
            2,0,0,0, % String's byte length, incl. nul.
            97, 0, % String data.
            0x47,0xcc,0x67,0x09, % ObjectID, time.
            0x34,0x75,0x06,      % ObjectID, machine.
            0x1e,0x3d,           % ObjectID, pid.
            0x95,0x36,0x9d,      % ObjectID, inc.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: db_pointer('a', '47cc67093475061e3d95369d')
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('symbol', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0E, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % String's byte length, incl. nul.
            97,116,111,109, 0, % String data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: symbol(atom)
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('mongostamp', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x11, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,0,0,0, 0,0,0,0, % Int64 mongostamp data. XXX Better ex. data.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: mongostamp(0)
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('min', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0xFF, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: min
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('max', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x7F, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: max
    ],
    bson_decoder:bson_to_term(Bson, Got).

test('invalid bson, missing terminating nul', [throws(bson_error(invalid))]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            32,0,0,0 % Int32 data.
        % Missing nul at end-of-doc.
    ],
    bson_decoder:bson_to_term(Bson, _Got).

:- end_tests('bson_decoder:bson_to_term/2').
