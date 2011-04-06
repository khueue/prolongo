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

test('invalid', [throws(bson_error(invalid))]) :-
    Term = invalid,
    bson_encoder:term_to_bson(Term, _Got).

:- end_tests('bson_encoder:term_to_bson/2').
