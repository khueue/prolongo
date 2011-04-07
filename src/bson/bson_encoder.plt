:- include(misc(common)).

:- begin_tests('bson_encoder:term_to_bson/2').

test('empty doc', [true(Got == Expected)]) :-
    Term =
    [
    ],
    Expected =
    [
        5,0,0,0, % Length of top doc.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('float', [true(Got == Expected)]) :-
    Term =
    [
        hello: 5.05
    ],
    Expected =
    [
        20,0,0,0, % Length of top doc.
        0x01, % Double tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            51,51,51,51, 51,51,20,64, % Double data, 5.05.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('string', [true(Got == Expected)]) :-
    Term =
    [
        'ä': 'ä\0ä'
    ],
    Expected =
    [
        19,0,0,0, % Length of top doc.
        0x02, % String tag.
            0xc3,0xa4, 0, % Ename, "ä\0".
            6,0,0,0, % String length incl. nul.
            0xc3,0xa4, 0, 0xc3,0xa4, 0, % String data, "ä\0ä\0".
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('int32 positive', [true(Got == Expected)]) :-
    Term =
    [
        hello: 32
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Int32 tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0, % Int32 data, 32.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('int32 negative', [true(Got == Expected)]) :-
    Term =
    [
        hello: -32
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Int32 tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            0xE0,0xFF,0xFF,0xFF, % Int32 data, -32.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('int64 positive', [true(Got == Expected)]) :-
    Term =
    [
        hello: 9223372036854775807
    ],
    Expected =
    [
        20,0,0,0, % Length of top doc.
        0x12, % Int32 tag.
            104,101,108,108,111, 0, % Ename.
            0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0x7F, % Int64 data.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('int64 negative', [true(Got == Expected)]) :-
    Term =
    [
        hello: -9223372036854775808
    ],
    Expected =
    [
        20,0,0,0, % Length of top doc.
        0x12, % Int64 tag.
            104,101,108,108,111, 0, % Ename.
            0,0,0,0, 0,0,0,0x80, % Int64 data.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('embedded doc', [true(Got == Expected)]) :-
    Term =
    [
        'BSON':
            [
                'a': 'awesome',
                'b': 5.05,
                'c': 1986
            ]
    ],
    Expected =
    [
        49,0,0,0, % Length of top doc.
        0x03, % Embedded doc tag.
            66,83,79,78, 0, % Ename "BSON\0".
            38,0,0,0, % Length of embedded doc (array).
            0x02, % String tag.
                97, 0, % Ename ("a\0").
                8,0,0,0, % String's byte length, incl. nul.
                97,119,101,115,111,109,101, 0, % String data, "awesome\0".
            0x01, % Double tag.
                98, 0, % Ename ("b\0").
                51,51,51,51,51,51,20,64, % Double 8-byte data, 5.05.
            0x10, % Int32 tag.
                99, 0, % Ename ("c\0").
                194,7,0,0, % Int32 data, 1986.
            0, % End of embedded doc (array).
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('embedded array', [true(Got == Expected)]) :-
    Term =
    [
        'BSON':
            [
                'awesome',
                5.05,
                1986
            ]
    ],
    Expected =
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
    bson_encoder:term_to_bson(Term, Got).

test('null', [true(Got == Expected)]) :-
    Term =
    [
        hello: null
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0x0A, % Null tag.
            104,101,108,108,111, 0, % Ename "hello\0".
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('boolean false', [true(Got == Expected)]) :-
    Term =
    [
        hello: false
    ],
    Expected =
    [
        13,0,0,0, % Length of top doc.
        0x08, % Boolean tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            0, % Boolean data, false.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('boolean true', [true(Got == Expected)]) :-
    Term =
    [
        hello: true
    ],
    Expected =
    [
        13,0,0,0, % Length of top doc.
        0x08, % Boolean tag.
            104,101,108,108,111, 0, % Ename "hello\0".
            1, % Boolean data, true.
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('undefined', [true(Got == Expected)]) :-
    Term =
    [
        hello: undefined
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0x06, % Undefined tag.
            104,101,108,108,111, 0, % Ename "hello\0".
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('min', [true(Got == Expected)]) :-
    Term =
    [
        hello: min
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0xFF, % Min tag.
            104,101,108,108,111, 0, % Ename "hello\0".
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('max', [true(Got == Expected)]) :-
    Term =
    [
        hello: max
    ],
    Expected =
    [
        12,0,0,0, % Length of top doc.
        0x7F, % Max tag.
            104,101,108,108,111, 0, % Ename "hello\0".
        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('invalid', [throws(bson_error(invalid))]) :-
    Term = invalid,
    bson_encoder:term_to_bson(Term, _Got).

:- end_tests('bson_encoder:term_to_bson/2').
