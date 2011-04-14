:- include(misc(common)).

:- begin_tests('bson:doc_bytes/2').

test('nonvar, nonvar') :-
    Doc =
    [
        hello - 256
    ],
    Bytes =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:doc_bytes(Doc, Bytes).

test('nonvar, var', [true(Got == Expected)]) :-
    Doc =
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
    bson:doc_bytes(Doc, Got).

test('var, nonvar', [true(Got == Expected)]) :-
    Expected =
    [
        hello - 256
    ],
    Bytes =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:doc_bytes(Got, Bytes).

test('var, var', [fail]) :-
    bson:doc_bytes(_, _).

:- end_tests('bson:doc_bytes/2').
