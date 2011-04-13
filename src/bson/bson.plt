:- include(misc(common)).

:- begin_tests('bson:term_bson/2').

test('nonvar, nonvar') :-
    Term =
    [
        hello = 256
    ],
    Bson =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:term_bson(Term, Bson).

test('nonvar, var', [true(Got == Expected)]) :-
    Term =
    [
        hello = 256
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:term_bson(Term, Got).

test('var, nonvar', [true(Got == Expected)]) :-
    Expected =
    [
        hello = 256
    ],
    Bson =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:term_bson(Got, Bson).

test('var, var', [fail]) :-
    bson:term_bson(_, _).

:- end_tests('bson:term_bson/2').
