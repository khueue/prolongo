:- module(_, []).
:- use_foreign_library(foreign(bson_bits)).

:- begin_tests(bson_bits).

test('bytes to float, 1', [true(Got == Expected)]) :-
    bytes_to_float(51,51,51,51, 51,51,20,64, Got),
    Expected = 5.05.

test('bytes to int32, 1', [true(Got == Expected)]) :-
    bytes_to_integer(0,0,0,128, Got),
    Expected = 2147483648.

test('bytes to int64, 1', [true(Got == Expected)]) :-
    bytes_to_integer(0,0,0,0, 0,0,0,128, Got),
    Expected = -9223372036854775808.

test('bytes to int64, 2', [true(Got == Expected)]) :-
    bytes_to_integer(0,0,0,0, 0,0,0,127, Got),
    Expected = 9151314442816847872.

:- end_tests(bson_bits).
