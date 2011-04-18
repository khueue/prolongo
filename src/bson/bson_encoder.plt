:- include(misc(common)).

:- begin_tests('bson_encoder:doc_to_bytes/2').

test('empty doc', [true(Got == Expected)]) :-
    Doc =
    [
    ],
    Expected =
    [
        5,0,0,0, % Length of top doc.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x01, float', [true(Got == Expected)]) :-
    Doc =
    [
        hello - 5.05
    ],
    Expected =
    [
        20,0,0,0, % Length of top doc.
        0x01, % Tag.
            104,101,108,108,111, 0, % Ename.
            51,51,51,51, 51,51,20,64, % Double data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x02, string', [true(Got == Expected)]) :-
    Doc =
    [
        'ä' - 'ä\0ä'
    ],
    Expected =
    [
        19,0,0,0, % Length of top doc.
        0x02, % Tag.
            0xc3,0xa4, 0, % Ename.
            6,0,0,0, % String length incl. nul.
            0xc3,0xa4, 0, 0xc3,0xa4, 0, % String data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x03, embedded doc', [true(Got == Expected)]) :-
    Doc =
    [
        hello -
            [
                'a' - 'awesome',
                'b' - 5.05,
                'c' - 1986
            ]
    ],
    Expected =
    [
        50,0,0,0, % Length of top doc.
        0x03, % Tag.
            104,101,108,108,111, 0, % Ename.
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
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x03, embedded empty doc', [true(Got == Expected)]) :-
    Doc =
    [
        hello - []
    ],
    Expected =
    [
        17,0,0,0, % Length of top doc.
        0x03, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % Length of embedded doc.
            0, % End of embedded doc.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x04, embedded array', [true(Got == Expected)]) :-
    Doc =
    [
        hello - ['awesome', 5.05, 1986]
    ],
    Expected =
    [
        50,0,0,0, % Length of top doc.
        0x04, % Tag.
            104,101,108,108,111, 0, % Ename.
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
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x05, binary, generic', [true(Got == Expected)]) :-
    Doc =
    [
        hello - binary(generic, [0,1,2,1,0])
    ],
    Expected =
    [
        22,0,0,0, % Length of top doc.
        0x05, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x00, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x05, binary, function', [true(Got == Expected)]) :-
    Doc =
    [
        hello - binary(function, [0,1,2,1,0])
    ],
    Expected =
    [
        22,0,0,0, % Length of top doc.
        0x05, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x01, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x05, binary, old generic', [true(Got == Expected)]) :-
    Doc =
    [
        hello - binary(old_generic, [0,1,2,1,0])
    ],
    Expected =
    [
        22,0,0,0, % Length of top doc.
        0x05, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x02, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x05, binary, uuid', [true(Got == Expected)]) :-
    Doc =
    [
        hello - binary(uuid, [0,1,2,1,0])
    ],
    Expected =
    [
        22,0,0,0, % Length of top doc.
        0x05, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x03, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x05, binary, md5', [true(Got == Expected)]) :-
    Doc =
    [
        hello - binary(md5, [0,1,2,1,0])
    ],
    Expected =
    [
        22,0,0,0, % Length of top doc.
        0x05, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x05, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x05, binary, user defined', [true(Got == Expected)]) :-
    Doc =
    [
        hello - binary(user_defined, [0,1,2,1,0])
    ],
    Expected =
    [
        22,0,0,0, % Length of top doc.
        0x05, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % Length of binary data.
            0x80, % Subtype.
            0,1,2,1,0, % Binary data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x06, undefined', [true(Got == Expected)]) :-
    Doc =
    [
        hello - +undefined
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0x06, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x07, object id', [true(Got == Expected)]) :-
    Doc =
    [
        hello - object_id('47cc67093475061e3d95369d')
    ],
    Expected =
    [
        24,0,0,0, % Length of top doc.
        0x07, % Tag.
            104,101,108,108,111, 0, % Ename.
            0x47,0xcc,0x67,0x09, % ObjectID, time.
            0x34,0x75,0x06,      % ObjectID, machine.
            0x1e,0x3d,           % ObjectID, pid.
            0x95,0x36,0x9d,      % ObjectID, inc.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x08, boolean false', [true(Got == Expected)]) :-
    Doc =
    [
        hello - +false
    ],
    Expected =
    [
        13,0,0,0, % Length of top doc.
        0x08, % Tag.
            104,101,108,108,111, 0, % Ename.
            0, % Boolean data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x08, boolean true', [true(Got == Expected)]) :-
    Doc =
    [
        hello - +true
    ],
    Expected =
    [
        13,0,0,0, % Length of top doc.
        0x08, % Tag.
            104,101,108,108,111, 0, % Ename.
            1, % Boolean data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x09, utc datetime', [true(Got == Expected)]) :-
    Doc =
    [
        hello - utc(1302354660284) % date(2011, 4, 9, ...)
    ],
    Expected =
    [
        20,0,0,0, % Length of top doc.
        0x09, % Tag.
            104,101,108,108,111, 0, % Ename.
            188,11,99,58,47,1,0,0, % UTC datetime data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x0A, null', [true(Got == Expected)]) :-
    Doc =
    [
        hello - +null
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0x0A, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x0B, regex', [true(Got == Expected)]) :-
    Doc =
    [
        hello - regex('a','i')
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x0B, % Tag.
            104,101,108,108,111, 0, % Ename.
            97, 0,  % Regex pattern.
            105, 0, % Regex options.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x0C, db pointer', [true(Got == Expected)]) :-
    Doc =
    [
        hello - db_pointer('a', '47cc67093475061e3d95369d')
    ],
    Expected =
    [
        30,0,0,0, % Length of top doc.
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
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x0D, js', [true(Got == Expected)]) :-
    Doc =
    [
        js - js('code ...')
    ],
    Expected =
    [
        22,0,0,0, % Length of top doc.
        0x0D, % Tag.
            106,115, 0, % Ename.
            9,0,0,0, % String's byte length, incl. nul.
            99,111,100,101,32,46,46,46, 0, % String data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x0E, symbol', [true(Got == Expected)]) :-
    Doc =
    [
        hello - symbol(atom)
    ],
    Expected =
    [
        21,0,0,0, % Length of top doc.
        0x0E, % Tag.
            104,101,108,108,111, 0, % Ename.
            5,0,0,0, % String's byte length, incl. nul.
            97,116,111,109, 0, % String data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x0F, js with scope', [true(Got == Expected)]) :-
    Doc =
    [
        js - js('code ...', [hello-32])
    ],
    Expected =
    [
        42,0,0,0, % Length of top doc.
        0x0F, % Tag.
            106,115, 0, % Ename.
            33,0,0,0, % Length of entire JS with scope.
            9,0,0,0, % String's byte length, incl. nul.
            99,111,100,101,32,46,46,46, 0, % String data.
                16,0,0,0, % Length of embedded doc.
                0x10, % Tag.
                    104,101,108,108,111, 0, % Ename.
                    32,0,0,0, % Int32 data.
                0, % End of embedded doc.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x10, int32', [true(Got == Expected)]) :-
    Doc =
    [
        hello - 32
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            32,0,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x11, mongostamp', [true(Got == Expected)]) :-
    Doc =
    [
        hello - mongostamp(0)
    ],
    Expected =
    [
        20,0,0,0, % Length of top doc.
        0x11, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,0,0,0, 0,0,0,0, % Int64 mongostamp data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x12, int64', [true(Got == Expected)]) :-
    Doc =
    [
        hello - 9223372036854775807
    ],
    Expected =
    [
        20,0,0,0, % Length of top doc.
        0x12, % Tag.
            104,101,108,108,111, 0, % Ename.
            0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0x7F, % Int64 data.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x12, int64 too large', [throws(bson_error(too_large,_))]) :-
    Doc =
    [
        hello - 9223372036854775808
    ],
    bson_encoder:doc_to_bytes(Doc, _Got).

test('0xFF, min', [true(Got == Expected)]) :-
    Doc =
    [
        hello - +min
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0xFF, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('0x7F, max', [true(Got == Expected)]) :-
    Doc =
    [
        hello - +max
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0x7F, % Tag.
            104,101,108,108,111, 0, % Ename.
        0 % End of top doc.
    ],
    bson_encoder:doc_to_bytes(Doc, Got).

test('invalid', [throws(bson_error(invalid))]) :-
    Doc = invalid_bson,
    bson_encoder:doc_to_bytes(Doc, _Got).

:- end_tests('bson_encoder:doc_to_bytes/2').
