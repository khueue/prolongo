:- module(_, []).
:- initialization(init_module).

init_module :-
    use_foreign_library(foreign(bson_bits)).

:- begin_tests(bson_bits).

test('byte handling') :-
    bson_bits:bytes_to_float(51,51,51,51,51,51,20,64, 5.05),
    bson_bits:bytes_to_integer(0,0,0,128, 2147483648),
    bson_bits:bytes_to_integer(0,0,0,0,0,0,0,128, -9223372036854775808),
    bson_bits:bytes_to_integer(0,0,0,0,0,0,0,127, 9151314442816847872).

:- end_tests(bson_bits).
