:- include(misc(common)).

:- begin_tests('bson:term_bson/2').

test('nonvar, nonvar') :-
    Term =
    [
        hello: 32
    ],
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Int32 tag
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0, % Int32 data, 32.
        0 % End of top doc.
    ],
    bson:term_bson(Term, Bson).

/* XXX Fix when implemented encoder.
test('nonvar, var', [true(Got == Expected)]) :-
    Term =
    [
        hello: 32
    ],
    Expected =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Int32 tag
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0, % Int32 data, 32.
        0 % End of top doc.
    ],
    bson:term_bson(Term, Got).
*/

test('var, nonvar', [true(Got == Expected)]) :-
    Expected =
    [
        hello: 32
    ],
    Bson =
    [
        xxx_not_impl,0,0,0, % Length of top doc.
        0x10, % Int32 tag
            104,101,108,108,111, 0, % Ename "hello\0".
            32,0,0,0, % Int32 data, 32.
        0 % End of top doc.
    ],
    bson:term_bson(Got, Bson).

test('var, var', [fail]) :-
    bson:term_bson(_, _).

:- end_tests('bson:term_bson/2').
