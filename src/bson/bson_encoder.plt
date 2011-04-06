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

test('string', [true(Got == Expected)]) :-
    Term =
    [
        a: b
    ],
    Expected =
    [
        14,0,0,0, % Length of top doc.

        0x02, % String tag.
            97, 0, % Ename, "a\0".
            2,0,0,0, % String length incl. nul.
            98, 0, % String data, "b\0".

        0 % End of top doc.
    ],
    bson_encoder:term_to_bson(Term, Got).

test('string utf8', [true(Got == Expected)]) :-
    Term =
    [
        ä: 'ä\0ä'
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
