:- include(misc(common)).

:- begin_tests('bson:pairs_bson/2').

test('nonvar, nonvar') :-
    Term =
    [
        hello - 256
    ],
    Bson =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:pairs_bson(Term, Bson).

test('nonvar, var', [true(Got == Expected)]) :-
    Term =
    [
        hello - 256
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:pairs_bson(Term, Got).

test('var, nonvar', [true(Got == Expected)]) :-
    Expected =
    [
        hello - 256
    ],
    Bson =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:pairs_bson(Got, Bson).

test('var, var', [fail]) :-
    bson:pairs_bson(_, _).

:- end_tests('bson:pairs_bson/2').

:- begin_tests('bson:assoc_bson/2').

test('nonvar, nonvar') :-
    Pairs =
    [
        hello - 256
    ],
    assoc:list_to_assoc(Pairs, Assoc),
    Bson =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:assoc_bson(Assoc, Bson).

test('nonvar, var', [true(Got == Expected)]) :-
    Pairs =
    [
        hello - 256
    ],
    assoc:list_to_assoc(Pairs, Assoc),
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:assoc_bson(Assoc, Got).

test('var, nonvar', [true(Got == Expected)]) :-
    Expected =
    [
        hello - 256
    ],
    Bson =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:assoc_bson(Assoc, Bson),
    assoc:assoc_to_list(Assoc, Got).

test('var, var', [fail]) :-
    bson:assoc_bson(_, _).

:- end_tests('bson:assoc_bson/2').
