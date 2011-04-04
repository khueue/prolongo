:- include(misc(common)).

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
        'ä': "ä\0ä"
    ],
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, _Got).

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
        hello: 32
    ],
    bson_decoder:decode(Bson, Got).

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
        hello: 32
    ],
    bson_decoder:decode(Bson, Got).

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
        hello: 5.05
    ],
    bson_decoder:decode(Bson, Got).

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
                '0': "awesome",
                '1': 5.05,
                '2': 1986
            ]
    ],
    bson_decoder:decode(Bson, Got).

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
                '0': "awesome",
                '1': 5.05,
                '2': 1986
            ]
    ],
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, Got).

test('binary, user defined', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x07, % ObjectID tag.
            66,83,79,78, 0, % Ename "BSON\0".
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
    bson_decoder:decode(Bson, Got).

test('js with scope', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0F, % JS with scope tag.
            106,115, 0, % Ename "js\0".
            6,6,6,6, % int xxx
            9,0,0,0, % String's byte length, incl. nul.
            99,111,100,101,32,46,46,46, 0, % String data, "code ...\0".
                xxx_not_impl,0,0,0, % Length of embedded doc.
                0x10, % Int32 tag
                    104,101,108,108,111, 0, % Ename "hello\0".
                    32,0,0,0, % Int32 data, 32.
                0, % End of embedded doc.
        0 % End of top doc.
    ],
    Expected =
    [
        js: js_with_scope("code ...", ['hello':32])
    ],
    bson_decoder:decode(Bson, Got).

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
    bson_decoder:decode(Bson, Got).

test('boolean true', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x08, % Boolean tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            1, % Boolean data, true.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: true
    ],
    bson_decoder:decode(Bson, Got).

test('boolean false', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x08, % Boolean tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            0, % Boolean data, false.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: false
    ],
    bson_decoder:decode(Bson, Got).

test('utc datetime', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x09, % UTC datetime tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            0,0,0,0, 0,0,0,0, % UTC datetime data, 0. XXX better
        0 % End of top doc.
    ],
    Expected =
    [
        hello: utc(0)
    ],
    bson_decoder:decode(Bson, Got).

test('boolean invalid', [throws(bson_error(invalid_boolean))]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x08, % Boolean tag
            104,101,108,108,111, 0, % Ename "hello\0".
            2, % Boolean data, INVALID.
        0 % End of top doc.
    ],
    bson_decoder:decode(Bson, _Got).

test('null', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0A, % Null tag.
            104,101,108,108,111, 0, % Ename "hello\0".
        0 % End of top doc.
    ],
    Expected =
    [
        hello: nil
    ],
    bson_decoder:decode(Bson, Got).

test('regex', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0B, % Regex tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            97, 0,  % Regex pattern, "a\0".
            105, 0, % Regex options, "i\0".
        0 % End of top doc.
    ],
    Expected =
    [
        hello: regex("a","i")
    ],
    bson_decoder:decode(Bson, Got).

test('db pointer', [true(Got == Expected)]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x0C, % DBPointer tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            2,0,0,0, % String's byte length, incl. nul.
            97, 0, % String data, "a\0".
            0x47,0xcc,0x67,0x09, % ObjectID, time.
            0x34,0x75,0x06,      % ObjectID, machine.
            0x1e,0x3d,           % ObjectID, pid.
            0x95,0x36,0x9d,      % ObjectID, inc.
        0 % End of top doc.
    ],
    Expected =
    [
        hello: db_pointer("a", '47cc67093475061e3d95369d')
    ],
    bson_decoder:decode(Bson, Got).

test('invalid bson, missing terminating nul', [throws(bson_error(invalid))]) :-
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Int32 tag
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0 % Int32 data, 32.
        % Missing nul at end-of-doc.
    ],
    bson_decoder:decode(Bson, _Got).

:- end_tests('bson_decoder:decode/2').
