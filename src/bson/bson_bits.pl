:- module(_,
[
    bytes_to_float/9,
    bytes_to_integer/5,
    bytes_to_integer/9
]).
:- use_foreign_library(foreign(bson_bits)).

:- begin_tests(bson_bits).

test('bytes to float, 1', [true(Got == Expected)]) :-
    Expected = 5.05,
    bytes_to_float(51,51,51,51, 51,51,20,64, Got).

test('bytes to int32, 1', [true(Got == Expected)]) :-
    Expected = 2147483648,
    bytes_to_integer(0,0,0,128, Got).

test('bytes to int64, 1', [true(Got == Expected)]) :-
    Expected = -9223372036854775808,
    bytes_to_integer(0,0,0,0, 0,0,0,128, Got).

test('bytes to int64, 2', [true(Got == Expected)]) :-
    Expected = 9151314442816847872,
    bytes_to_integer(0,0,0,0, 0,0,0,127, Got).

:- end_tests(bson_bits).
