% :- set_prolog_flag(iso, true).
:- encoding(utf8).

:- use_module(bson).

run :-
    nl,
    ignore(bson:test_decode),
    nl.
